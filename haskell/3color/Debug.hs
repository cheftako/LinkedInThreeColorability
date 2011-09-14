module Debug where

import qualified Data.IntMap as M

import Base
import qualified CSP as C
import CSP (CSP)
import Graph (Graph)
import Solver

-- | Dot representation of a solution
dotSolution :: Assignment -> CSP -> String
dotSolution asn csp = "graph g {\n" ++ ns ++ es ++ "}"
  where ns = concatMap showNode (C.vars csp)
        es = concatMap showEdge (C.constraints csp)
        showNode v = '\t' : show v ++ " [label=\"" ++ nlab v ++ "\",style=filled,color=" ++ color v ++ "]\n"
        showEdge ((v,c1), (w,c2))
            | v < w && c1 == c2 && c1 == R = '\t' : show v ++ " -- " ++ show w ++ "\n"
            | otherwise = ""
        color v = case M.lookup v asn of
                    Just x  -> colors !! colorToInt x
                    Nothing -> error $ "Debug.dotSolution: color not found for: " ++ show v
        colors = ["Red","Green","Blue"]
        nlab = idToName
