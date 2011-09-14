{-# LANGUAGE BangPatterns #-}

-- | Utilities for solver implementations

module Solver (
      ReduceState(..)
    , Assignment
    , mkReduceState
    , construct
    , reduce
    , pushUnresolved
    , pushResolved
    , modCSP
    , showSolution
    ) where

import Control.Monad.Logic
import Data.Function (on)
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Data.List (foldl', foldl1')
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

import Base
import CSP
import Util

type Unresolved = (Var, [(Color, ConstraintSet)])

-- | Map of variables to assigned colors
type Assignment = IntMap Color

data ReduceState = RS
    -- | Has the csp or removed vars stack been modified?
    { rsModified   :: !Bool

    -- | The current csp state
    , rsCSP        :: !CSP

    -- | Map of variables that have been resolved
    , rsResolved   :: !Assignment

    -- | Stack of variables that have not been resolved
    , rsUnresolved :: ![Unresolved]

    } deriving Show

-- | Constructs an initial reduce state for the given CSP.
mkReduceState :: CSP -> ReduceState
mkReduceState csp = RS { rsModified   = False
                       , rsCSP        = csp
                       , rsResolved   = IM.empty
                       , rsUnresolved = []
                       }

-- | Reduces the CSP as far as possible and returns the updated CSP and
--   the stack of removed variables.
reduce :: ReduceState -> ReduceState
reduce rs
    | rsModified rs' = reduce rs'
    | otherwise      = rs'
    where
      rs' = reducePass rs

-- | Runs the CSP through a single pass of all reductions
reducePass :: ReduceState -> ReduceState
reducePass rs = foldl' (flip ($)) (resetModified rs)
                    [ lemma2
                    , lemma3
                    , lemma4
                    , lemma2
                    , lemma5
                    , lemma6
                    , lemma2
                    ]

-- | Resets the ReduceState modified flag while in the State monad
resetModified :: ReduceState -> ReduceState
resetModified rs = rs { rsModified = False }

-- | Pushes a resolved variable and an updated CSP into ReduceState
pushResolved :: Var -> Color -> ReduceState -> ReduceState
pushResolved v c rs = rs { rsModified = True
                         , rsCSP      = delVar v $ setVarColor v c (rsCSP rs)
                         , rsResolved = IM.insert v c (rsResolved rs) }

-- | Pushes an unresolved variable and an updated CSP into ReduceState
pushUnresolved :: Var -> ReduceState -> ReduceState
pushUnresolved v rs = rs { rsModified   = True
                         , rsCSP        = delVar v csp
                         , rsUnresolved = u : rsUnresolved rs }
    where
      csp  = rsCSP rs
      u    = (v, varCons v csp)

-- | Applies a function to the csp in the ReduceState
modCSP :: (CSP -> CSP) -> ReduceState -> ReduceState
modCSP f rs = rs { rsModified = True
                 , rsCSP      = let !x = f (rsCSP rs) in x }

-- | Lemma 2 reduction: if a variable can only be colored in two ways then it
--   can  be removed to create a simpler problem by propagating its conflicts to
--   its neighbors.
lemma2 :: ReduceState -> ReduceState
lemma2 rs = foldl' f rs $ do
                let csp = rsCSP rs
                v <- vars csp
                case varColors v csp of
                    [c1, c2] -> return (v, c1, c2)
                    _        -> mzero
    where
      f a cns@(v, _, _)     = pushUnresolved v $ modCSP (g cns) a
      g cns csp             = insCons (mkCns cns csp) csp
      mkCns (v, c1, c2) csp = (cartesianProduct `on`
                                (\x -> S.toList $ varColorCons v x csp)) c1 c2

-- | Lemma 3 reduction: if (v, X) and (w, Y) are two (var, color) pairs such
--   that their only conflicts are ((v, X), (w, Z)) with Z /= Y or
--   ((v, Z), (w, Y)) with Z /= X then color using these pairs and remove them
--   from the csp.
lemma3 :: ReduceState -> ReduceState
lemma3 rs = foldl' f rs $ do
              let csp = rsCSP rs
              (v, c) <- varColorList csp
              (v', cs') <- getOneVarConstraint v c csp
              case getConstrainedTo v' cs' csp of
                  Just c' -> [(v, c), (v', c')]
                  _       -> mzero
    where
      getOneVarConstraint v c csp
          = case S.toList $ varColorCons v c csp of
              [] -> mzero
              ys
                | all ((v ==) . fst) ys -> return (fst $ head ys, map snd ys)
                | otherwise             -> mzero

      getConstrainedTo _  []     _   = Nothing
      getConstrainedTo v' (c:cs) csp
          = case getOneVarConstraint v' c csp of
              [] -> getConstrainedTo v' cs csp
              _  -> Just c

      f a (v, c)
          | notMember v csp = a
          | otherwise       = pushResolved v c a
          where
            csp = rsCSP a

-- | Lemma 4 reduction: if the constraints of a (var, color) pair is a superset
--   of the constraints of another (var, color) pair then remove the more
--   restrictive pair.
lemma4 :: ReduceState -> ReduceState
lemma4 rs = foldl' f rs (vars $ rsCSP rs)
    where
      f a v = g v (combinations $ varColors v (rsCSP a)) a

      g _ []            a = a
      g v ((c1, c2):cs) a
        | cons2 `S.isSubsetOf` cons1 = modCSP (delVarColor v c1) a
        | cons1 `S.isSubsetOf` cons2 = modCSP (delVarColor v c2) a
        | otherwise                  = g v cs a
        where
          csp   = rsCSP a
          cons1 = varColorCons v c1 csp
          cons2 = varColorCons v c2 csp

-- | Lemma 5 reduction: if any (var, color) pair is not involved in a
--   constraint then the variable can  be removed from the instance and
--   colored.
lemma5 :: ReduceState -> ReduceState
lemma5 rs = foldl' f rs $ do
                let csp = rsCSP rs
                v <- vars csp
                c <- varColors v csp
                if S.null (varColorCons v c csp)
                  then return (v, c)
                  else mzero
    where
      f a (v, c)
        | member v csp = pushResolved v c a
        | otherwise    = a
        where
          csp = rsCSP a

-- | Lemma 6 reduction: if a (var, color) pair is involved in a constraint with
--   all colors of another variable then remove color from the variable's
--   availability set. Lemma 2 can be applied to remove it (because it has only
--   two colors).
--
--   Note: this does not strictly follow the definition of lemma 6, which seems
--   erroneous. It claims that a variable can be removed in a (a,2)-CSP if it
--   is constrained by 3 colors of another variable, but in such a CSP there
--   could be a fourth constraint.
lemma6 :: ReduceState -> ReduceState
lemma6 rs' = f (vars $ rsCSP rs') rs'
    where
      f []     rs = rs
      f (v:vs) rs
          = case cns of
              [] -> f vs rs
              _  -> case cns' of
                      []        -> f vs rs
                      ((v2, c2):_) -> modCSP (delVarColor v2 c2) rs
        where
          csp  = rsCSP rs
          cns  = map (\c -> varColorCons v c csp) (varColors v csp)
          cns' = S.toList $ foldl1' (flip S.intersection) cns

construct :: ReduceState -> Maybe Assignment
construct rs = constructSub us asn
    where
      us = rsUnresolved rs
      asn = rsResolved rs

constructSub :: [Unresolved] -> Assignment -> Maybe Assignment
constructSub [] asn = Just asn
constructSub ((v,cs):vs) asn
    | null colors = Nothing
    | otherwise   = constructSub vs (IM.insert v (head colors) asn)
    where
      colors = resolveVar cs asn

resolveVar :: [(Color, ConstraintSet)] -> Assignment -> [Color]
resolveVar cs asn = mapMaybe findAvailable cs
    where
      findAvailable (c, cons)
        | any hasConflict (S.toList cons) = Nothing
        | otherwise                       = Just c
      hasConflict (w, y) = IM.lookup w asn == Just y

-- | Prints a solution in the format required for the contest
showSolution :: Assignment -> String
showSolution = unlines . map showVar . IM.assocs
    where
      showVar (vid, color) = idToName vid ++ ':' : showColor color
      showColor = show . (1+) . colorToInt
