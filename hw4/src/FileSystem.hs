{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module FileSystem where

import           Control.Lens     (Traversal', filtered, has, makeLenses, makePrisms,
                                   traversed, (%~), (&), (^.), (^..), (^?), _head)
import           Control.Monad    (forM, unless)
import           Data.Maybe       (isJust)
import           System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import           System.FilePath  (joinPath, splitDirectories, (-<.>), (</>))

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

traverseAll :: Traversal' FS FilePath
traverseAll argBind obj@(Dir _ _) =
    Dir <$> argBind (obj^.name) <*> traverse (traverseAll argBind) (obj^.contents)
traverseAll argBind obj@(File _) = File <$> argBind (obj^.name)

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

--move :: FilePath -> Traversal' FS FS
--move path argBind fs = (fs ) & name %~ (path </>)
