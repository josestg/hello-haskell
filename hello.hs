import Data.List (intersperse, nub, singleton, sort)
import Data.Map qualified as M

doubleMe x = 2 * x

doubleMeStream n = [2 * x | x <- [1 .. n]]

doubleOddsStream n = [2 * x | x <- [1 .. n], x `mod` 2 == 1]

doubleOrTripple n = [if odd x then 2 * x else 3 * x | x <- [1 .. n]]

length' xs = [1 | _ <- xs]

removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase xs = [x | x <- xs, x `elem` ['A' .. 'Z']]

head' :: [a] -> a
head' [] = error "Cannot get head of empty list"
head' (x : _) = x

maxvalue :: (Ord a) => [a] -> a
maxvalue [] = error "Cannot get max value of empty list"
maxvalue [x] = x
maxvalue (x : xs)
  | x > tailmax = x
  | otherwise = tailmax
  where
    tailmax = maxvalue xs

replicate' :: Int -> a -> [a]
replicate' n a
  | n <= 0 = []
  | otherwise = a : replicate' (n - 1) a

take' :: Int -> [a] -> [a]
take' _ [] = []
take' n _ | n <= 0 = []
take' n (x : xs) = x : take (n - 1) xs

takecaseof :: Int -> [a] -> [a]
takecaseof n xs =
  case (n, xs) of
    (0, _) -> []
    (n, []) -> []
    (n, x : xs) ->
      if n < 0
        then []
        else x : take (n - 1) xs

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

zipcaseof :: [a] -> [b] -> [(a, b)]
zipcaseof xs ys =
  case (xs, ys) of
    ([], _) -> []
    (_, []) -> []
    (x : xs, y : ys) -> (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (x : xs)
  | x == e = True
  | otherwise = e `elem'` xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' [x] = [x]
reverse' (x : xs) = reverse' xs ++ [x]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallest = quicksort [e | e <- xs, e <= x]
      biggest = quicksort [e | e <- xs, e > x]
   in smallest ++ [x] ++ biggest

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

zipWithCaseof :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithCaseof f xs ys =
  case (f, xs, ys) of
    (_, _, []) -> []
    (_, [], _) -> []
    (_, x : xs, y : ys) -> f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f a b = g
  where
    g = f b a

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

filter' :: (Eq a, Ord a) => (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x : xs)
  | f x = x : rest
  | otherwise = rest
  where
    rest = filter' f xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x : xs) =
  let smallest = quicksort' (filter' (<= x) xs)
      biggest = quicksort' (filter' (> x) xs)
   in smallest ++ [x] ++ biggest

-- https://en.wikipedia.org/wiki/Collatz_conjecture
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (n * 3 + 1)

numLongChain :: Int
numLongChain = length (filter' isLong (map chain [1 .. 100]))
  where
    isLong xs = length xs < 15

elemfoldl :: (Eq a) => a -> [a] -> Bool
elemfoldl y = foldl (\acc x -> acc || x == y) False

filterfoldl :: (Eq a, Ord a) => (a -> Bool) -> [a] -> [a]
filterfoldl p = foldl (\acc x -> if p x then acc ++ [x] else acc) []

filterfoldr :: (Eq a, Ord a) => (a -> Bool) -> [a] -> [a]
filterfoldr p = foldr (\x acc -> if p x then x : acc else acc) []

-- Using $ function application
quicksort'' :: (Ord a) => [a] -> [a]
quicksort'' [] = []
quicksort'' (x : xs) =
  let smallest = quicksort'' $ filter' (<= x) xs
      biggest = quicksort'' $ filter' (> x) xs
   in smallest ++ [x] ++ biggest

sortedUnique :: (Eq a, Ord a) => [a] -> [a]
sortedUnique = sort . nub

dotIntersperse :: [Char] -> [Char]
dotIntersperse = intersperse '.'

phoneBook :: [(String, String)]
phoneBook =
  [ ("betty", "555-2938"),
    ("bonnie", "452-2928"),
    ("patsy", "493-2928"),
    ("lucille", "205-2928"),
    ("wendy", "939-8282"),
    ("penny", "853-2492")
  ]

findByKey :: (Eq k) => [(k, v)] -> k -> Maybe v
findByKey [] key = Nothing
findByKey ((k, v) : xs) key =
  if key == k
    then Just v
    else findByKey xs key

findByKeyFoldr :: (Eq k) => [(k, v)] -> k -> Maybe v
findByKeyFoldr xs key = g key xs
  where
    g key = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing

fromList' :: (Ord k) => [(k, v)] -> M.Map k v
fromList' = foldr (\(k, v) acc -> M.insert k v acc) M.empty

data Point = Point Float Float deriving (Show)

data Shape
  = Circle Point Float
  | Rectangle Point Point
  deriving (Show)

surface :: Shape -> Float
surface s = case s of
  (Circle _ r) -> pi * r ^ 2
  (Rectangle (Point x1 y1) (Point x2 y2)) -> abs (x2 - x1) * abs (y2 - y1)

-- Record

data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float
  }
  deriving (Eq, Show, Read)

bob :: Person
bob = Person {firstName = "Bob", lastName = "Alex", age = 25, height = 168}

-- Type Parameters
data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
vplus (Vector x y z) (Vector p q r) = Vector (x * p) (y * z) (z * r)

-- Recursive data structures

data Tree a = EmptyTree | Node {v :: a, l :: Tree a, r :: Tree a} deriving (Show, Read, Eq)

newTree :: a -> Tree a
newTree x = Node x EmptyTree EmptyTree

insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree x EmptyTree = newTree x
insertTree x (Node a l r)
  | x == a = Node x l r
  | x < a = Node a (insertTree x l) r
  | otherwise = Node a l (insertTree x r)

treeToListInOrder:: Tree a -> [a]
treeToListInOrder EmptyTree = []
treeToListInOrder Node{v, l, r} = treeToListInOrder l ++ [v] ++ treeToListInOrder r

treeToListPreOrder:: Tree a -> [a]
treeToListPreOrder EmptyTree = []
treeToListPreOrder Node{v, l, r} = v : treeToListPreOrder l ++ treeToListPreOrder r

treeToListPostOrder:: Tree a -> [a]
treeToListPostOrder EmptyTree = []
treeToListPostOrder Node{v, l, r} = treeToListPostOrder l ++ treeToListPostOrder r ++ [v]

simpleTree :: Tree Integer
simpleTree = foldr insertTree EmptyTree [8, 6, 1, 3, 2, 7, 5]

-- Typeclaseses

data TrafficLight = Red |Yellow | Green

class ColorEq a where
    (===) :: a -> a -> Bool
    (/==) :: a -> a -> Bool
    x === y = not (x /== y)
    x /== y = not (x === y )

instance ColorEq TrafficLight where
    Red === Red = True
    Green === Green = True
    Yellow === Yellow = True
    _ === _ = False

instance Show TrafficLight where
    show Red = "RED"
    show Green = "GREEN"
    show Yellow = "YELLOW"

-- The Functor typeclass

class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

instance Functor' [] where
    fmap' = map'