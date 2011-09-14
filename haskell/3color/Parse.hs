module Parse (load, parse) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.List (foldl')

import Data.List.Split (wordsBy)

import Base

load :: FilePath -> IO (Gr () ())
load fp = fmap parse (readFile fp)

parse :: String -> Gr () ()
parse str = foldl' (flip parseEdge) empty es
    where
      ls = lines str
      n  = read (head ls)
      es = take n (tail ls)

parseEdge :: String -> Gr () () -> Gr () ()
parseEdge str gr = foldl' f (insNode (v, ()) gr) es
    where
      f acc w = insEdge (v, w, ()) acc
      str_es  = wordsBy (== ':') str
      v       = nameToId $ head str_es
      es      = filter (v >) es'
      es'     = map nameToId $ wordsBy (== ',') (concat $ tail str_es)
