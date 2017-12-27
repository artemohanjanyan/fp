{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}

module FileSystem
    ( FS(..)
    , name
    , contents
    , _Dir
    , _File

    , scanDir

    , cd
    , ls
    , file

    , changeExtensions
    , traverseAll
    , rm

    , walker
    ) where

import           Control.Lens           (Traversal', filtered, has, makeLenses,
                                         makePrisms, traversed, (%~), (&), (^.), (^..),
                                         (^?), _head)
import           Control.Monad          (forM, unless, void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, ask, runReaderT)
import           Control.Monad.State    (MonadState, StateT, get, gets, put, runStateT)
import qualified Data.List.NonEmpty     as L
import           Data.Maybe             (isJust)
import           System.Directory       (doesDirectoryExist, doesFileExist, listDirectory)
import           System.FilePath        (joinPath, splitDirectories, (-<.>), (</>))

data FS
    = Dir
        { _name     :: FilePath  -- название папки, не полный путь
        , _contents :: [FS]
        }
    | File
        { _name     :: FilePath  -- название файла, не полный путь
        }
    deriving (Show)

scanDir :: FilePath -> IO FS
scanDir path = do
    isPathDir <- doesDirectoryExist path
    unless isPathDir $ fail $ path ++ " is not a directory"
    content <- listDirectory path
    let currentDir = last $ splitDirectories path
    contents <- forM content $ \dirEntry -> do
        let entryPath = path </> dirEntry
        isEntryFile <- doesFileExist entryPath
        if isEntryFile
            then pure $ File dirEntry
            else scanDir entryPath
    pure $ Dir currentDir contents

makeLenses ''FS
makePrisms ''FS

cd :: FilePath -> Traversal' FS FS
cd path = contents.traversed.filtered filterDir
  where
    filterDir :: FS -> Bool
    filterDir fs = isJust $ fs ^? filtered (has _Dir) . name . filtered (== path)

ls :: Traversal' FS FilePath
ls = contents.traversed.name

file :: FilePath -> Traversal' FS FilePath
file filename = contents.traversed.filtered filterFile.name
  where
    filterFile :: FS -> Bool
    filterFile fs = isJust $ fs ^? _File . filtered (== filename)

changeExtensions :: String -> FS -> FS
changeExtensions newExt fs =
    fs & contents.traversed.filtered (has _File).name %~ (-<.> newExt)

-- Traversal is better than list, (^..traverseAll) gives list
traverseAll :: Traversal' FS FilePath
traverseAll argBind obj@(Dir _ _) =
    Dir <$> argBind (obj^.name) <*> traverse (traverseAll argBind) (obj^.contents)
traverseAll argBind obj@(File _) = File <$> argBind (obj^.name)

-- Meh
rm :: FilePath -> FS -> FS
rm _ fs@(File _) = fs
rm path fs =
    case path' of
        []       -> fs
        [dir]    -> Dir (fs^.name)
                    (fs^..contents.traversed.filtered
                        (\fs' -> has _File fs
                              || (fs'^.name) /= dir
                              || (fs'^.name == dir
                                  && isJust (fs'^?contents.filtered (has _head)))))
        dir:rest ->
            let dirFS = fs^?contents.traversed.filtered
                         (\fs' -> has _Dir fs' && (fs'^.name) == dir)
            in case dirFS of
                Nothing     -> fs
                Just dirFS' -> Dir (fs^.name)
                           (rm (joinPath rest) dirFS' :
                            (fs^..contents.traversed.filtered
                             (\fs' -> has _File fs' || (fs'^.name) /= dir)))
  where
    path' = splitDirectories path

walker :: FilePath -> IO ()
walker path = do
    fs <- scanDir path
    let (files, dirs) = getStats fs
    void $ runReaderT (runStateT loop $ (path, files, dirs) L.:| []) fs
  where
    loop :: StateT (L.NonEmpty (FilePath, Int, Int)) (ReaderT FS IO) ()
    loop = do
        printState
        cmd <- readCmd
        case cmd of
            Just cmd' -> processCmd cmd'
            Nothing   -> liftIO $ putStrLn "incorrect command"
        loop

data Cmd = CmdCd FilePath | CmdUp

readCmd :: MonadIO m => m (Maybe Cmd)
readCmd = do
    liftIO $ putStr "> "
    cmd <- liftIO getLine
    case words cmd of
        ["cd", path] -> pure $ Just $ CmdCd path
        ["up"]       -> pure $ Just CmdUp
        _            -> pure Nothing

processCmd ::
    ( MonadState (L.NonEmpty (FilePath, Int, Int)) m
    , MonadReader FS m
    , MonadIO m
    ) => Cmd -> m ()
processCmd (CmdCd path) = do
    fs <- ask
    stack <- get
    fsAfterCd <-
        case fs ^? foldr (flip (.) . (\(dir, _, _) -> cd dir)) id (L.init stack) of
            Just fs' -> pure fs'
            Nothing  -> fail "incorrect stack state"
    case fsAfterCd ^? cd path of
        Just fs' -> do
            let (files, dirs) = getStats fs'
            let (_, stackFiles, stackDirs) = L.head stack
            put $ (path, stackFiles + files, stackDirs + dirs) L.<| stack
        Nothing -> liftIO $ putStrLn "no such directory"
processCmd CmdUp = do
    stack <- get
    case L.nonEmpty $ L.tail stack of
        Nothing     -> liftIO $ putStrLn "can't up from root"
        Just stack' -> put stack'

printState ::
    ( MonadState (L.NonEmpty (FilePath, Int, Int)) m
    , MonadReader FS m
    , MonadIO m
    ) => m ()
printState = do
    currentDir <- getCurrentDir
    liftIO $ putStrLn $ "You in " ++ show currentDir
    (root, _, _) <- gets L.last
    (_, files, dirs) <- gets L.head
    liftIO $ putStrLn $ "Files from root  " ++ show root ++ ": " ++ show files
    liftIO $ putStrLn $ "Directories from " ++ show root ++ ": " ++ show dirs

getCurrentDir ::
    ( MonadState (L.NonEmpty (FilePath, Int, Int)) m
    , MonadReader FS m
    ) => m FilePath
getCurrentDir = do
    stack <- get
    pure $ foldr1 (flip (</>)) $ L.map (\(dir, _, _) -> dir) stack

getStats :: FS -> (Int, Int)
getStats fs =
    ( length $ fs^..contents.traversed.filtered (has _File)
    , length $ fs^..contents.traversed.filtered (has _Dir )
    )
