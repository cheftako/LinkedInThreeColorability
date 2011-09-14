module Util (
      hasRunOf
    , hasLength
    , bipartition
    , checkBipartite
    , combinations
    , cartesianProduct
    , swap
    ) where

import qualified Data.IntMap as M
import Data.List (foldl')
import qualified Data.Set as S
import Data.Set (Set)

-- | True if the list has n of any x in the list of xs. This algorithm is best
--   for small lists and will not work for infinite lists.
hasRunOf :: Int -> [Int] -> Bool
hasRunOf n xs = f xs M.empty
    where
      f []     _ = False
      f (y:ys) m = case M.insertLookupWithKey (\_ -> (+)) y 1 m of
                      (Just z,  m') -> (z + 1 == n) || f ys m'
                      (Nothing, m') -> f ys m'

hasLength :: Int -> [a] -> Bool
hasLength 0 []     = True
hasLength 0 _      = False
hasLength _ []     = False
hasLength n (_:xs) = hasLength (n-1) xs
{-# INLINE hasLength #-}

bipartition :: Ord a => [[a]] -> (Set a, Set a)
bipartition xs = f xs (S.empty, S.empty)
    where
      f []       a        = a
      f [x]      (ls, rs) = (ls', rs)
        where
          ls' = foldl' (flip S.insert) ls x

      f (x:y:ys) (ls, rs) = f ys (ls', rs')
        where
          ls' = foldl' (flip S.insert) ls x
          rs' = foldl' (flip S.insert) rs y

checkBipartite :: Ord a => (a -> Set a) -> (Set a, Set a) -> Bool
checkBipartite f (as, bs) = check bs && check as
    where
      check xs = all (\x -> S.null (f x `S.intersection` xs)) (S.toList xs)

-- | Returns the order-independent combinations of the argument
combinations :: [a] -> [(a, a)]
combinations = concat . f
    where
      f [] = []
      f (x:xs) = map ((,) x) xs : f xs

cartesianProduct :: [a] -> [a] -> [(a,a)]
cartesianProduct x y = [ (x', y') | x' <- x, y' <- y ]

-- | Swaps the elements of a 2-tuple
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)
