module Base where

import Control.Arrow (first)
import Data.Char (chr, ord)
import Data.List (foldl', unfoldr)

import Util

type Color = Int

{-
data Color = R | G | B | Y
  deriving (Enum, Eq, Ord, Show)
-}

colors3 :: [Color]
colors3 = take 3 colors4

colors4 :: [Color]
colors4 = [0..4]

-- | Encode a variable name as an ID
nameToId :: String -> Int
nameToId "" = error "nameToId: empty string"
nameToId x = foldl' (\a y -> a * 26 + (ord y - 64)) 0 x

-- | Decode a variable ID as a String
idToName :: Int -> String
idToName 0 = error "idToName: 0 value"
idToName x = reverse $ unfoldr go x
    where
      go 0 = Nothing
      go y = Just . first (chr . (+) 65) . swap . (`divMod` 26) $ y - 1

colorToInt :: Color -> Int
colorToInt = id
{-# INLINE colorToInt #-}

intToColor :: Int -> Color
intToColor = id
{-# INLINE intToColor #-}
