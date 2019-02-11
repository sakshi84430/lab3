--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab 3: Recursive and higher-order functions                                --
--------------------------------------------------------------------------------

module Lab3 where

--------------------------------------------------------------------------------

-- Some of the functions we will be defining as part of this lab are
-- part of Haskell's standard library. The following line tells the compiler
-- not to import them.
import Prelude hiding ( Monoid(..), elem, maximum, intersperse, transpose
                      , subsequences, permutations, any, all, flip, takeWhile
                      , zipWith, groupBy, notElem )

import Data.List (delete)

--------------------------------------------------------------------------------
-- Recursive and higher-order functions

-- solved using if/then/else
elem :: Eq a => a -> [a] -> Bool
elem _ []     = False
elem a (x:xs) = if a==x then True else elem a xs

-- solved using guards
elem' :: Eq a => a -> [a] -> Bool
elem' _ []     = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = elem' a xs

-- solved using logical or
elem'' :: Eq a => a -> [a] -> Bool
elem'' _ []     = False
elem'' a (x:xs) = a==x || elem'' a xs

-- solved using foldr
elem''' :: Eq a => a -> [a] -> Bool
elem''' a = foldr (\x r -> a==x || r) False

-- solved using function composition
elem'''' :: Eq a => a -> [a] -> Bool
elem'''' x = not . null . filter (==x)

-- solved using if/then/else
maximum :: Ord a => [a] -> a
maximum [x]    = x
maximum (x:xs) = let y = maximum xs
                 in if x>y then x else y

-- solved using guards
maximum' :: Ord a => [a] -> a
maximum' [x]    = x
maximum' (x:xs)
    | x>y       = x
    | otherwise = y
    where y = maximum' xs

-- solved using max
maximum'' :: Ord a => [a] -> a
maximum'' [x]    = x
maximum'' (x:xs) = max x (maximum'' xs)

-- solved using foldr1
maximum''' :: Ord a => [a] -> a
maximum''' = foldr1 max

-- intersperse using explicit recursion
intersperse :: a -> [a] -> [a]
intersperse x []     = []
intersperse x (y:ys) = y : prepend ys
    where
        prepend []     = []
        prepend (z:zs) = x : z : prepend zs

-- any using explicit recursion
any :: (a -> Bool) -> [a] -> Bool
any p []     = False
any p (x:xs) = p x || any p xs

-- any using foldr
any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr (\x r -> p x || r) False

-- all using explicit recursion
all :: (a -> Bool) -> [a] -> Bool
all p []     = True
all p (x:xs) = p x && all p xs

-- all using foldr
all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr (\x r -> p x && r) True

-- flip just exchanges the arguments
flip :: (a -> b -> c) -> b -> a -> c
flip f b a = f a b

-- takeWhile using explicit recursion
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x:xs)
    | p x       = x : takeWhile p xs
    | otherwise = []

-- zipWith using explicit recursion
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f []     _      = []
zipWith f _      []     = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

-- zipWith in terms of map and zip
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = map (uncurry f) (zip xs ys)

-- groupBy can be implemented easily with the help of `span`
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy r []     = []
groupBy r (x:xs) = (x:ys) : groupBy r zs
    where (ys,zs) = span (r x) xs

-- subsequences using explicit recursion, append, and map
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = ys ++ map (x:) ys
    where ys = subsequences xs

-- permutations using explicit recursion
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = go xs []
    where go []     _  = []
          go (y:ys) zs = map (y:) (permutations (zs++ys)) ++ go ys (y:zs)

-- more elegant solution for permutations which is implemented with the help of
-- the `delete` function from Data.List, which requires the Eq constraint.
permutations' :: Eq a => [a] -> [[a]]
permutations' [] = [[]]
permutations' xs = [x : ys | x <- xs, ys <- permutations' (delete x xs)]

--------------------------------------------------------------------------------
-- Monoids

-- Monoid laws:
--
-- (Left identity)      mappend mempty x = x
-- (Right identity)     mappend x mempty = x
-- (Associativity)      mappend x (mappend y z) = mappend (mappend x y) z
-- (mconcat)            mconcat = foldr mappend mempty

class Monoid a where
    mempty  :: a
    mappend :: a -> a -> a
    mconcat :: [a] -> a
    mconcat = foldr mappend mempty

instance Monoid Int where
    mempty  = 0
    mappend = (+)

instance Monoid [a] where
    mempty  = []
    mappend = (++)

instance Monoid b => Monoid (a -> b) where
    mempty  = \a -> mempty
    f `mappend` g = \a -> f a `mappend` g a

--------------------------------------------------------------------------------
