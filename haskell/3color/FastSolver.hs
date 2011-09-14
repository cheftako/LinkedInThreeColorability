{-# LANGUAGE BangPatterns #-}

module FastSolver (
      Assignment
    , solve
    , showSolution
    ) where

import Data.Bits
import Control.Arrow (first, second)
import Control.Monad.Logic
import qualified Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree
import qualified Data.Graph.Inductive.Query as G
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Data.List ((\\), delete, find, foldl', nub)
import Data.Maybe (fromJust, fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Tree as T

import Base
import CSP
import CSPSearch
import Solver
import Util

{------------------------------------------------------------------------------
 - Lemmas
 -----------------------------------------------------------------------------}

-- | Solves for a subset of lemma8 (isolated constraints with variables that
--   both have three available colors). The remaining half of lemma 8 is
--   addressed by a more general lemma 9.
lemma8 :: ReduceState -> [ReduceState]
lemma8 rs
    | null isolated = []
    | otherwise = go isolated
    where
      csp = rsCSP rs
      isolated = isolatedConstraints csp
      go [] = []
      go ((vc@(v, _), vc'@(v', _)):cns)
        | lvc v == 3 && lvc v' == 3 = [coalesce vc vc' rs]
        | otherwise = go cns
      lvc v = length (varColors v csp)

lemma9 :: ReduceState -> [ReduceState]
lemma9 rs
    | null dangling = []
    | otherwise = go (head dangling)
    where
      csp = rsCSP rs
      dangling = danglingConstraints csp
      go cn = [splitOne cn rs, splitOne (swap cn) rs]
      splitOne ((v,c), _) = modCSP $ setVarColor v c

-- | Solve for implied constraint cycles
lemma10a :: ReduceState -> [ReduceState]
lemma10a rs
    | null cycles = []
    | otherwise = go (head cycles)
    where
      cycles = implicationCycles (rsCSP rs)
      go cyc
        | null outsideCns = [chooseCycle rs]
        | otherwise = [chooseCycle rs, avoidCycle rs]
        where
          outsideCns :: [VarColor]
          outsideCns = concatMap
                          (\((v1,c1)) ->
                              filter ((`notElem` cycleVars) . fst) 
                                     (S.toList $ varColorCons v1 c1 (rsCSP rs)))
                          cyc
          cycleVars = map fst cyc
          chooseCycle = modCSP (setCycleColors . delOutsideCns)
          setCycleColors csp = foldr (uncurry setVarColor) csp cyc
          delOutsideCns csp = foldr (uncurry delVarColor) csp outsideCns
          avoidCycle = modCSP (\csp -> foldr (uncurry delVarColor) csp cyc)

-- | Solve for single implied constraint
lemma10b :: ReduceState -> [ReduceState]
lemma10b rs = case implications (rsCSP rs) of
                  Nothing -> []
                  Just x  -> f x
    where
      f ((v,c),(v',c')) = fmap (`modCSP` rs)
                              [ setVarColor v' c'
                              , delVarColor v c . delVarColor v' c'
                              ]

-- | Solve for semi-implied constraints. A semi-implied constraint occurs when
--   a (v,c) pair eliminates two options of a 4-color (v, c) pair.
lemma10c :: ReduceState -> [ReduceState]
lemma10c rs = case semiImplications (rsCSP rs) of
                  Nothing -> []
                  Just x  -> f x
    where
      f (v,[c1,c2],[c3,c4]) = fmap (`modCSP` rs)
                                    [ delVarColor v c1 . delVarColor v c2
                                    , delVarColor v c3 . delVarColor v c4
                                    ]
      f r = error $ "FastSolver.lemma10c: should never get here: " ++ show r

lemma11 :: ReduceState -> [ReduceState]
lemma11 rs = case constrained of
              Nothing    -> []
              Just (v,c) -> fmap (`modCSP` rs)
                              [ setVarColor v c
                              , delVarColor v c
                              ]
    where
      csp = rsCSP rs
      constrained = f (vars csp)
      f :: [Var] -> Maybe VarColor
      f [] = Nothing
      f (v:vs)
        | vc_len == 3 = matchVar v 4 `mplus` f vs
        | vc_len == 4 = matchVar v 3 `mplus` f vs
        | otherwise = f vs
        where
          vc_len = length $ varColors v csp

      matchVar :: Var -> Int -> Maybe VarColor
      matchVar v ncons = listToMaybe $ mapMaybe (g ncons v) (varColors v csp)
      g ncons v c | S.size (varColorCons v c csp) >= ncons = Just (v,c)
                  | otherwise                              = Nothing

-- Note: vc below has 3 constraints (not 4, because it would have been removed
-- by lemma 11). vc2 must have 2 constraints because if it were 1 it would have
-- been removed by lemma 8 or lemma 9, and if it were 3 or more it would have
-- been removed by lemma 11.
lemma12 :: ReduceState -> [ReduceState]
lemma12 rs = f $ do
                let csp = rsCSP rs
                vc@(v, c) <- varColorList csp

                let cns = varColorCons v c csp
                when (S.size cns /= 3) mzero
 
                vc2@(v2, c2) <- S.toList cns
                when (length (varColors v2 csp) /= 4) mzero

                let vc3@(v3, c3) = S.findMin $ S.delete vc $ varColorCons v2 c2 csp
                -- Do vc, vc2, vc3 form a cycle?
                if S.member vc $ varColorCons v3 c3 csp
                  then return (vc, Just (vc2, vc3))
                  else return (vc, Nothing)
    where
      f []    = []
      f (x:_) = case x of
                  ((v,c), Just ((v2, c2), (v3, c3))) ->
                        fmap (`modCSP` rs)
                            [ setVarColor v  c
                            , setVarColor v2 c2
                            , setVarColor v3 c3
                            ]
                  ((v, c), Nothing) ->
                        fmap (`modCSP` rs)
                            [ delVarColor v c
                            , setVarColor v c
                            ]

-- Very similar to lemma12, but concerns itself with a remote constraint
-- involving a variable with only 3 colors.
lemma13 :: ReduceState -> [ReduceState]
lemma13 rs = f $ do
                let csp = rsCSP rs
                vc@(v, c) <- varColorList csp

                let cns = varColorCons v c csp
                when (S.size cns /= 3) mzero
 
                vc2@(v2, c2) <- S.toList cns
                let cns2 = varColorCons v2 c2 csp
                when (S.size cns2 /= 2) mzero

                let vc3@(v3, c3) = S.findMin $ S.delete vc cns2
                -- Do vc, vc2, vc3 form a cycle?
                if S.member vc (varColorCons v3 c3 csp) &&
                     S.size (varColorCons v3 c3 csp) == 3
                  then return (vc, Just (vc2, vc3))
                  else return (vc, Nothing)

    where
      f []    = []
      f (x:_) = case x of
                  ((v,c), Just ((v2, c2), (v3, c3))) ->
                        fmap (`modCSP` rs)
                            [ setVarColor v  c
                            , setVarColor v2 c2
                            , setVarColor v3 c3
                            ]
                  ((v, c), Nothing) ->
                        fmap (`modCSP` rs)
                            [ delVarColor v c
                            , setVarColor v c
                            ]

lemma15 :: ReduceState -> [ReduceState]
lemma15 rs = fromMaybe [] $ msum [lemma15a cmpts rs, lemma15b cmpts rs]
    where
      cmpts = smallThreeComponents (rsCSP rs)

-- | Handles k = 12 components
-- TODO: need a way to signal that the instance is unsolveable!
-- Unsolveable can only be detected if we get to the otherwise clause and then
-- can't find a match
lemma15a :: [[VarColor]] -> ReduceState -> Maybe [ReduceState]
lemma15a cmpts rs = listToMaybe $ filter (not . null) $ map f cmpts
    where
      f vcs
        | length vcs /= 12 = mzero
        | otherwise = firstMatch $ do
            (vc1, vc1cns) <- choose v1
            (vc2, vc2cns) <- choose v2
            (vc3, vc3cns) <- choose v3
            (vc4, vc4cns) <- choose v4

            let all_cns = S.unions [vc1cns, vc2cns, vc3cns, vc4cns]
                all_vcs = [vc1, vc2, vc3, vc4]
            if all (`S.notMember` all_cns) all_vcs
              then return $! foldl' (flip setVC) rs all_vcs
              else mzero
        where
          csp = rsCSP rs
          vs = nub (map fst vcs)
          [v1,v2,v3,v4] = vs
          choose v = map (\c -> ((v, c), varColorCons v c csp)) (varColors v csp)
          setVC (v, c) = modCSP (setVarColor v c)

          firstMatch []     = []
          firstMatch (x:_) = [x]

-- | Handles k = 8 components
lemma15b :: [[VarColor]] -> ReduceState -> Maybe [ReduceState]
lemma15b cmpts rs = listToMaybe $ mapMaybe f cmpts
    where
      f vcs
        | length vcs /= 8    = mzero
        | isBipartite bp csp = Just [foldl' (\a (v, c) -> modCSP (setVarColor v c) a) rs (S.toList $ fst bp)]
        | otherwise          = let !(x, y) = findK8Crossover csp
                               in Just $ fmap (`modCSP` rs)
                                            [ uncurry setVarColor x . uncurry setVarColor y
                                            , uncurry delVarColor x
                                            , uncurry delVarColor y
                                            ]
        where
          csp = subgraph (S.fromList vcs) (rsCSP rs)
          bp = bipartition . T.levels . head . dff $ csp

lemma17 :: ReduceState -> [ReduceState]
lemma17 rs = case witnesses (rsCSP rs) of
              []        -> []
              ((v,c):_) -> fmap (`modCSP` rs)
                              [ setVarColor v c
                              , delVarColor v c
                              ]

lemma18a :: ReduceState -> [ReduceState]
lemma18a rs = case fiveVarLargeTwoComponents (rsCSP rs) of
                ([(v,cv),(w, cw),(x, cx),(y, cy),_]:_) ->
                  fmap (`modCSP` rs)
                    [ setVarColor w cw
                    , setVarColor x cx
                    , setVarColor y cy . setVarColor v cv
                    ]
                _ -> []

lemma18b :: ReduceState -> [ReduceState]
lemma18b rs = case sandwichedLargeTwoComponents (rsCSP rs) of
                ([(v1, c1), (v2, c2)]:_) -> fmap (`modCSP` rs)
                                              [ setVarColor v1 c1
                                              , setVarColor v2 c2
                                              ]
                _ -> []

lemma18c :: ReduceState -> [ReduceState]
lemma18c rs = case largeTwoComponents (rsCSP rs) of
                  (xs:_) -> f xs
                  []     -> []
    where
      f :: Component -> [ReduceState]
      f xs
        | length xs == 4 = fmap (`modCSP` rs)
                              [ uncurry setVarColor vc1 . uncurry setVarColor vc3
                              , uncurry setVarColor vc2 . uncurry setVarColor vc4
                              ]
        where
          [vc1,vc2,vc3,vc4] = xs
      f xs
        | length xs `elem` [8, 12]
            = [modCSP ( uncurry setVarColor vc1
                      . uncurry setVarColor vc3
                      . uncurry setVarColor vc6
                      . uncurry setVarColor vc8) rs]
        where
          [vc1,_,vc3,_,_,vc6,_,vc8] = xs
      f xs = error $ "lemma18c: " ++ show rs ++ "\n" ++ show xs

{------------------------------------------------------------------------------
 - Lemma Helpers
 -----------------------------------------------------------------------------}

coalesce :: VarColor -> VarColor -> ReduceState -> ReduceState
coalesce (v1, c1) (v2, c2) rs = rs3
  where
    csp = rsCSP rs

    !rs1 = pushUnresolved v1 rs
    !rs2 = pushUnresolved v2 rs1
    !rs3 = modCSP (insCons (v1cns ++ v2cns') . insVar v3 colors4) rs2

    -- We reuse the first variable's id. This is safe since we will
    -- overwrite the coalesced variable's color during constrution.
    v3  = v1

    v1cns  = filter ((/= v2) . fst . snd) $ cnsList v1 (delete c1 $ varColors v1 csp)
    v2cns  = filter ((/= v1) . fst . snd) $ cnsList v2 (delete c2 $ varColors v2 csp)
    v2cns' = map (first (second remapColor)) v2cns
    cnsList v = concatMap
                  (\c -> zip (repeat (v3, c)) (S.toList $ varColorCons v c csp))

    remapColor c = fromJust $ lookup c avail
    avail = zip (filter (/= c2) (varColors v2 csp))
                (colors4 \\ filter (/= c1) (varColors v1 csp))

-- | Find a constraint shared by two (v, c) pairs such that it is the only
--   constraint for those two pairs.
isolatedConstraints :: CSP -> [(VarColor, VarColor)]
isolatedConstraints csp = filter filt $ danglingConstraints csp
    where
      filt (_,(v, c)) = S.size (varColorCons v c csp) == 1

-- | Find a constraint shared by two (v,c) pairs such that it is the only
--   constraint for the first of those pairs. This is a superset of "isolated
--   constraints".
danglingConstraints :: CSP -> [(VarColor, VarColor)]
danglingConstraints csp = foldr go [] $ varColorList csp
    where
      go vc@(v, c) a
        | S.size cs == 1 = (vc, con) : a
        | otherwise = a
        where
          cs = varColorCons v c csp
          con = S.findMin cs

-- | Finds situations where one (v,c) pair implies another (v,c) pair. This
--   search limits itself to where the first (v,c) pair has at least two
--   constraints on the second, which is satisfactory for our needs.
implications :: CSP -> Maybe (VarColor, VarColor)
implications csp = listToMaybe . concatMap f . varColorList $ csp
    where
      f vc@(v,c) = zip (repeat vc) (varColorImplications v c csp)

-- | Returns a list of semi-implied constraints. A semi-implied constraint occurs
--   when a (v,c) pair eliminates two options of a 4-color (v, c) pair.
semiImplications :: CSP -> Maybe (Var, [Color], [Color])
semiImplications csp = listToMaybe $ mapMaybe prune (concatMap candidates $ varColorList csp)
    where
      candidates (v, c) = implicationCandidates v c csp
      prune (v, cs)     = case varColors v csp \\ cs of
                            cs'@[_,_] -> Just (v, cs', cs)
                            _         -> Nothing

implicationCandidates :: Var -> Color -> CSP -> [(Var, [Color])]
implicationCandidates v c csp = candidates
    where
      candidates = IM.toList $ IM.filter (hasLength 2) consMap

      consMap = flatten $ varColorCons v c csp
      flatten = S.fold (\(v', c') -> IM.insertWith (++) v' [c']) IM.empty

varColorImplications :: Var -> Color -> CSP -> [VarColor]
varColorImplications v c csp = mapMaybe prune $ implicationCandidates v c csp
    where
      prune (v', cs') = case varColors v' csp \\ cs' of
                          [c'] -> Just (v', c')
                          _   -> Nothing

implicationGraph :: CSP -> Gr VarColor ()
implicationGraph csp = G.mkGraph nodes edges
    where
      vcs   = varColorList csp
      nodes = map (\vc -> (vcToInt vc, vc)) vcs
      edges = concatMap (\vc@(v,c) ->
                              zip3
                                (repeat (vcToInt vc))
                                (map vcToInt (varColorImplications v c csp))
                                (repeat ()))
              vcs

-- | Returns (v,c) pairs involved in a cycle. The order of the pairs themselves
--   do not necessarily form a path.
implicationCycles :: CSP -> [[VarColor]]
implicationCycles = map (map intToVc) . filter ((1 <) . length) . G.scc . implicationGraph

-- There has to be a better way to find the crossing in lemma15...
findK8Crossover :: CSP -> (VarColor, VarColor)
findK8Crossover csp
    = case mapMaybe f (vars csp) of
          [x,y] -> let !vc@(v,c) = fromJust $ find ((== x) . fst) (varColorList csp)
                       !vc' = S.findMin $ S.filter ((== y) . fst) (varColorCons v c csp)
                   in (vc, vc')
          x     -> error $ "Should never happen: " ++ show x
    where
      bp = bipartition . T.levels . head . dff
      f v
        | isBipartite (bp csp') csp' = Just v
        | otherwise                  = Nothing
        where
          csp' = delVar v csp

isBipartite :: (Set VarColor, Set VarColor) -> CSP -> Bool
isBipartite sets csp = checkBipartite (\(v, c) -> varColorCons v c csp) sets


{------------------------------------------------------------------------------
 - Solve
 -----------------------------------------------------------------------------}

solve :: CSP -> Maybe Assignment
solve = listToMaybe . observeMany 1 . simplify . mkReduceState

simplify :: ReduceState -> Logic Assignment
simplify rs = f rs [ lemma8
                   , lemma9
                   , lemma10a
                   , lemma10b
                   , lemma10c
                   , lemma11
                   , lemma12
                   , lemma13
                   , lemma15
                   , lemma17
                   , lemma18a
                   , lemma18b
                   , lemma18c
                   ]
    where
      f rs' [] = case match rs' of
                  Just rs'' -> case construct rs'' of
                                Just x  -> return x
                                Nothing -> mzero
                  Nothing   -> mzero
      f rs' (g:gs) = case g rs' of
                      [] -> f rs' gs
                      xs -> msum $ map (simplify . reduce) xs

match :: ReduceState -> Maybe ReduceState
match rs = case resolveColors (rsCSP rs) of
              Nothing  -> Nothing
              Just vcs -> Just $ foldl' (flip $ uncurry pushResolved) rs vcs

resolveColors :: CSP -> Maybe [VarColor]
resolveColors csp
    | isEmpty csp              = Just []
    | IM.size asn == length vs = Just $ IM.toList asn
    | otherwise                = Nothing
    where
      vs           = vars csp
      (_, var_end) = varRange csp
      cmpt_start   = var_end + 1
      cmpts        = IM.fromList $ zip [cmpt_start..] (components csp)
      cmpt_ids     = IM.keys cmpts
      (s, t)       = (maxBound - 1, maxBound)
      gr           = flowGraph s t vs cmpt_ids
      gr'          = G.delNodes [s, t] $ G.mf gr s t
      asn          = flowToAssignment gr' cmpts

flowToAssignment :: Gr () (Int, Int, Int) -> IntMap Component -> Assignment
flowToAssignment gr cmpts = foldl' f IM.empty (G.labEdges gr)
    where
      f a (v, c, (_, flow, _))
        | flow == 1 = IM.insert v (findColor v c) a
        | otherwise = a
 
      findColor v c = snd $ head $ filter ((== v) . fst) $ cmpts IM.! c

flowGraph :: Int -> Int -> [Var] -> [Int] -> Gr () Int
flowGraph s t vs cmpt_ids = G.mkGraph ns es
    where
      ns             = zip (s:t:(vs ++ cmpt_ids)) (repeat ())
      es             = concat [s_to_vars, vars_to_cmpts, cmpts_to_t]
      s_to_vars      = zip3 (repeat s) vs (repeat 1)
      vars_to_cmpts  = [ (v, c, 1) | v <- vs, c <- cmpt_ids ]
      cmpts_to_t     = zip3 cmpt_ids (repeat t) (repeat 1)

{------------------------------------------------------------------------------
 - Utilities
 -----------------------------------------------------------------------------}

-- Note: this assumes that the number of vertices are less than (x - 2) where
-- x is the number of bits in the Int datatype.
vcToInt :: VarColor -> Int
vcToInt (v,c) = shiftL v 2 + fromEnum c

intToVc :: Int -> VarColor
intToVc i = (shiftR i 2, toEnum $ i .&. 0x3)
