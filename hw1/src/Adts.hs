{-# LANGUAGE DeriveFunctor #-}
module Adts
    ( Day (..)
    , nextDay
    , afterDays
    , isWeekend
    , daysToParty

    , Creature (..)
    , Knight (..)
    , Monster (..)
    , fight

    , Vector (..)
    , getX
    , getY
    , getZ
    , euclidNorm
    , add
    , scalar
    , neg
    , dist
    , vectorProduct

    , Nat (..)

    , Tree (..)
    , treeElem
    , treeInsert
    , fromList
    ) where

import           Patterns (mergeSort)

import           GHC.Real (Ratio ((:%)))
import           Data.Semigroup (Semigroup (..))

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- Возвращает следующий за переданным день недели.
nextDay :: Day -> Day
nextDay Sun = Mon
nextDay day = succ day

-- Возвращает день недели, который наступит после заданного через переданное
-- число дней.
afterDays :: Day -> Int -> Day
afterDays day dayN = afterDays' day (dayN `mod` 7)
  where
    afterDays' day' dayN' = iterate nextDay day' !! dayN'

-- Проверяет, является ли день недели выходным.
isWeekend :: Day -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _   = False

-- Выводит число дней, оставшихся до пятницы.
daysToParty :: Day -> Int
daysToParty Fri = 0
daysToParty day = daysToParty (nextDay day) + 1


class Creature a where
    health :: a -> Int
    attack :: a -> Int
    courage :: a -> Integer
    hit :: Int -> a -> a

data Knight = Knight
    { name         :: String
    , rank         :: String
    , knightHealth :: Int
    , knightAttack :: Int
    } deriving (Show, Eq)

instance Creature Knight where
    health = knightHealth
    attack = knightAttack
    courage _ = 9000
    hit dmg knight = knight { knightHealth = max (health knight - dmg) 0 }

data Monster = Monster
    { monsterHealth :: Int
    , monsterAttack :: Int
    } deriving (Show, Eq)

instance Creature Monster where
    health = monsterHealth
    attack = monsterAttack
    courage _ = 3
    hit dmg monster = monster { monsterHealth = max (health monster - dmg) 0 }

fight :: (Creature a, Creature b) => a -> b -> (Either a b, Int)
fight c1 c2
    | courage c1 > courage c2 = makeStat $ fight' c1 c2
    | otherwise               = makeStat $ flipEither $ fight' c2 c1
  where
    fight' :: (Creature a, Creature b) => a -> b -> Either a b
    fight' c1' c2' =
        let newC2 = hit (attack c1') c2'
        in if health newC2 > 0
            then flipEither $ fight' newC2 c1'
            else Left c1'

    flipEither :: Either a b -> Either b a
    flipEither (Left x)  = Right x
    flipEither (Right x) = Left x

    makeStat (Left x)  = (Left x,  health c1 - health x)
    makeStat (Right x) = (Right x, health c2 - health x)


data Vector
    = Vector2D Double Double
    | Vector3D Double Double Double
    deriving (Show, Eq)

getX :: Vector -> Double
getX (Vector2D x _)   = x
getX (Vector3D x _ _) = x

getY :: Vector -> Double
getY (Vector2D _ y)   = y
getY (Vector3D _ y _) = y

getZ :: Vector -> Double
getZ (Vector2D _ _)   = 0
getZ (Vector3D _ _ z) = z

euclidNorm :: Vector -> Double
euclidNorm v = sqrt $ getX v ** 2 + getY v ** 2 + getZ v ** 2

add :: Vector -> Vector -> Vector
add (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (x1 + x2) (y1 + y2)
add vec1 vec2 = Vector3D
        (getX vec1 + getX vec2)
        (getY vec1 + getY vec2)
        (getZ vec1 + getZ vec2)

scalar :: Vector -> Vector -> Double
scalar vec1 vec2 = getX vec1 * getX vec2
        + getY vec1 * getY vec2
        + getZ vec1 * getZ vec2

neg :: Vector -> Vector
neg (Vector2D x y)   = Vector2D (-x) (-y)
neg (Vector3D x y z) = Vector3D (-x) (-y) (-z)

dist :: Vector -> Vector -> Double
dist vec1 vec2 = euclidNorm $ add vec1 (neg vec2)

vectorProduct :: Vector -> Vector -> Vector
vectorProduct vec1 vec2 = Vector3D
        (getY vec1 * getZ vec2 - getZ vec1 * getY vec2)
        (getZ vec1 * getX vec2 - getX vec1 * getZ vec2)
        (getX vec1 * getY vec2 - getY vec1 * getX vec2)


data Nat = Z | S Nat deriving Show

instance Eq Nat where
    Z    == Z    = True
    S n1 == S n2 = n1 == n2
    _    == _    = False

instance Ord Nat where
    Z    <= _    = True
    S n1 <= S n2 = n1 <= n2
    _    <= Z    = False

instance Num Nat where
    Z    + n  = n
    S n1 + n2 = n1 + S n2

    Z    * _  = Z
    S n1 * n2 = n2 + n1 * n2

    abs n = n

    signum Z = Z
    signum _ = S Z

    fromInteger 0 = Z
    fromInteger n
        | n > 0     = S $ fromInteger $ n - 1
        | otherwise = error "fromInteger (negative integer) :: Nat"

    n    - Z    = n
    S n1 - S n2 = n1 - n2
    Z    - _    = error "Z - S n"

instance Real Nat where
    toRational = (GHC.Real.:% 1) . toInteger

instance Enum Nat where
    toEnum = fromInteger . toEnum
    fromEnum = fromEnum . toInteger

instance Integral Nat where
    toInteger Z     = 0
    toInteger (S n) = 1 + toInteger n

    a `quotRem` b
        | a >= b =
            let (quot', rem') = (a - b) `quotRem` b
            in (quot' + 1, rem')
        | otherwise = (Z, a)

    divMod = quotRem

-- ‘even’ and ‘gcd’ defined in ‘GHC.Real’ now work


data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Functor, Show, Eq)

instance Foldable Tree where
    foldr _   init' Leaf         = init'
    foldr acc init' (Node x l r) = foldr acc (acc x (foldr acc init' r)) l

-- ‘null’ and ‘length’ defined in ‘Data.Foldable’ now work

treeElem :: Ord a => a -> Tree a -> Bool
treeElem _ Leaf = False
treeElem y (Node x left right)
    | y == x    = True
    | y < x     = treeElem y left
    | otherwise = treeElem y right

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert y Leaf = Node y Leaf Leaf
treeInsert y tree@(Node x left right)
    | y == x    = tree
    | y < x     = Node x (treeInsert y left) right
    | otherwise = Node x left (treeInsert y right)

fromList :: Ord a => [a] -> Tree a
fromList xs = fromSorted $ mergeSort xs
  where
    fromSorted [] = Leaf
    fromSorted list = Node x left right
      where
        (part1, x:part2) = splitAt (length list `div` 2) list
        left = fromSorted part1
        right = fromSorted part2

instance Semigroup (Tree a) where
    (<>) = mappend

instance Monoid (Tree a) where
    mempty = Leaf
    mappend Leaf a = a
    mappend a Leaf = a
    mappend a (Node x l r) = Node x (mappend a l) r
