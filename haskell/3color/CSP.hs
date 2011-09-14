{-# LANGUAGE BangPatterns #-}

module CSP (
    -- * Types
      CSP
    , Var
    , VarColor
    , ConstraintSet

    -- * Constructors
    , fromGraph
    , empty

    -- * Graph Queries
    , isEmpty
    , vars
    , varRange
    , varColors
    , varCons
    , varColorCons
    , constraints
    , varColorList
    , varColorSet
    , member
    , notMember
    , subgraph
    , matchVarColor

    -- * Update
    , insVar
    , insVars
    , delVar
    , setVarColor
    , delVarColor
    , insCon
    , insCons
    , delCon
    , delCons
    )
    where

import Control.Arrow (first, second)
import qualified Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Set (Set)

import Base

type Var = Int

type VarColor = (Var, Color)

type ConstraintSet = Set VarColor

type ConstraintMap = IntMap ConstraintSet

type CSPRep = IntMap ([Color], ConstraintMap)

newtype CSP = CSP CSPRep

instance Show CSP where
    showsPrec _ = showsCSP

fromGraph :: Gr () () -> CSP
fromGraph gr = addCons . addVars $ empty
    where
      addVars  = insVars (G.nodes gr) colors3
      addCons  = insCons (concatMap f $ G.edges gr)
      f (v, w) = [ ((v, c), (w, c)) | c <- colors3 ]

empty :: CSP
empty = CSP IM.empty

isEmpty :: CSP -> Bool
isEmpty = IM.null . getCSP

vars :: CSP -> [Var]
vars = IM.keys . getCSP

varRange :: CSP -> (Var, Var)
varRange (CSP csp) = (fst $ IM.findMin csp, fst $ IM.findMax csp)

varColors :: Var -> CSP -> [Color]
varColors v = fst . fromMaybe missing_err . IM.lookup v . getCSP
    where
      missing_err = error $ "CSP.varColors: no var with id: " ++ show v

varCons :: Var -> CSP -> [(Color, ConstraintSet)]
varCons v = fromMaybe missing_err . fmap (flatten . snd) . IM.lookup v . getCSP
    where
      flatten = map (first intToColor) . IM.assocs
      missing_err = error $ "CSP.varCons: no var with id: " ++ show v

varColorCons :: Var -> Color -> CSP -> ConstraintSet
varColorCons v c csp = cs
    where
      cm = fromMaybe missing_err (IM.lookup v (getCSP csp))
      cs = fromMaybe S.empty (IM.lookup (colorToInt c) (snd cm))
      missing_err = error $ "CSP.varColorCons: no var with id: " ++ show v

constraints :: CSP -> [(VarColor, VarColor)]
constraints = IM.foldWithKey f [] . getCSP
    where
      f k v a   = IM.foldWithKey (g k) a (snd v)
      g x k v a = S.fold (\vc -> (:) ((x, intToColor k), vc)) a v

varColorList :: CSP -> [VarColor]
varColorList = concat . IM.foldWithKey f [] . getCSP
    where
      f k v a = zip (repeat k) (fst v) : a

varColorSet :: CSP -> Set VarColor
varColorSet = S.fromList . varColorList

member :: Var -> CSP -> Bool
member x = IM.member x . getCSP

notMember :: Var -> CSP -> Bool
notMember x = not . member x

subgraph :: Set VarColor -> CSP -> CSP
subgraph vcs csp = foldl' f csp (varColorList csp)
    where
      f a vc@(v, c)
        | vc `S.member` vcs = a
        | otherwise         = delVarIfEmpty v $ delVarColor v c a
      delVarIfEmpty v a
        | null (varColors v a) = delVar v a
        | otherwise            = a

insVar :: Var -> [Color] -> CSP -> CSP
insVar x cs = modCSP $ IM.insert x cm
    where
      cm = (cs, IM.fromList $ zip (map colorToInt cs) (repeat S.empty))

insVars :: [Var] -> [Color] -> CSP -> CSP
insVars xs cs csp = foldl' (\a x -> insVar x cs a) csp xs

delVar :: Var -> CSP -> CSP
delVar x = modCSP (IM.delete x) . delConflicts
    where
      delConflicts csp = foldl' f csp (varColors x csp)
      f a c = delVarColor x c a

setVarColor :: Var -> Color -> CSP -> CSP
setVarColor v c = insVar v [c] . delVar v . adjustColors
    where
      -- When we set the color for a variable, we must remove available colors
      -- from other variables with a constraint on this color.
      adjustColors csp = S.fold (uncurry delVarColor) csp (varColorCons v c csp)

delVarColor :: Var -> Color -> CSP -> CSP
delVarColor v c = modCSP delVC . delNbrs
    where
      delNbrs csp = S.fold (`delOneCon` (v, c)) csp (varColorCons v c csp)
      delVC       = IM.adjust f v
      f (cs, cns) = (filter (/= c) cs, IM.delete (colorToInt c) cns)

insCon :: VarColor -> VarColor -> CSP -> CSP
insCon !vc1 !vc2 !csp = modCSP (insOneConRaw vc2 vc1 . insOneConRaw vc1 vc2) csp

insCons :: [(VarColor, VarColor)] -> CSP -> CSP
insCons !cs !csp = foldl' (flip $ uncurry insCon) csp cs

insOneConRaw :: VarColor -> VarColor -> CSPRep -> CSPRep
insOneConRaw !(v1, c1) !vc2@(v2, _) !csp
    -- | v1 `notMember` csp = error $ "CSP.rawInsOneCon: no var with id: " ++ show v1
    | v1 == v2           = csp
    | otherwise          = IM.adjust f v1 csp
    where
      c1_int = colorToInt c1
      f cm = let !x = second (IM.adjust (S.insert vc2) c1_int) cm in x
--        | IM.notMember c1_int (snd cm) = error $ "CSP.rawInsOneCon: no pair: " ++ show vc1 
--        | otherwise = let !x = second (IM.adjust (S.insert vc2) c1_int) cm in x

delCon :: VarColor -> VarColor -> CSP -> CSP
delCon vc1 vc2 = delOneCon vc2 vc1 . delOneCon vc1 vc2

delCons :: [(VarColor, VarColor)] -> CSP -> CSP
delCons cs csp = foldl' (flip $ uncurry delCon) csp cs

delOneCon :: VarColor -> VarColor -> CSP -> CSP
delOneCon (v1, c1) vc2 = modCSP $ IM.adjust f v1
    where
      f = second $ IM.adjust g (colorToInt c1)
      g = S.delete vc2

showsCSP :: CSP -> ShowS
showsCSP (CSP csp) z = IM.foldWithKey f z csp
    where
      f x cm a = ('\n':) $ shows x $ IM.foldWithKey g a (snd cm)
      g x cs = ("\n    "++) . shows (intToColor x) . (" -> "++) . shows cs

getCSP :: CSP -> CSPRep
getCSP (CSP !x) = x

modCSP :: (CSPRep -> CSPRep) -> CSP -> CSP
modCSP f (CSP !x) = let !x' = f x in CSP x'

matchVarColor :: VarColor -> CSP -> (Maybe ConstraintSet, CSP)
matchVarColor (v, c) csp = (cs, csp')
    where
      cs = do
              cm <- IM.lookup v (getCSP csp)
              IM.lookup (colorToInt c) (snd cm)
      csp' = delVarColor v c csp
