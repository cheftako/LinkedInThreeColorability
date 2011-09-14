module CSPSearch
    ( Component
    , dff
    , components
    , smallThreeComponents
    , witnesses
    , largeTwoComponents
    , fiveVarLargeTwoComponents
    , sandwichedLargeTwoComponents
    )
    where

import Control.Monad (msum)
import Data.Function (on)
import Data.List (nub)
import Data.Maybe (isNothing, isNothing, mapMaybe)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, ViewR(..), (><))
import qualified Data.Set as S
import qualified Data.Tree as T
import Data.Tree (Tree(..))

import CSP

type Component = [VarColor]

-- | Depth first forest for a CSP
dff :: CSP -> [Tree VarColor]
dff csp = fst $ dffSearch (varColorList csp) csp

dffSearch :: [VarColor] -> CSP -> ([Tree VarColor], CSP)
dffSearch []       csp               = ([], csp)
dffSearch _        csp | isEmpty csp = ([], csp)
dffSearch (vc:vcs) csp
    = case matchVarColor vc csp of
        (Nothing, _   ) -> dffSearch vcs csp
        (Just cs, csp2) -> (Node vc ts : ts', csp4)
            where
              (ts,  csp3) = dffSearch (S.toList cs) csp2
              (ts', csp4) = dffSearch vcs           csp3

bfl :: VarColor -> CSP -> [(VarColor, Int)]
bfl vc csp = bflSearch (Seq.singleton (vc, 0)) csp []

bflSearch :: Seq (VarColor, Int) -> CSP -> [(VarColor, Int)] -> [(VarColor, Int)]
bflSearch q csp acc
    | Seq.null q     = acc
    | isNothing nbrs = bflSearch q'' csp  acc
    | otherwise      = bflSearch q'' csp' acc'
    where
      (q' :> vcl@(vc, l)) = Seq.viewr q
      (nbrs, csp')        = matchVarColor vc csp
      acc'                = vcl : acc
      q'' = case nbrs of
              Nothing -> q'
              Just xs -> Seq.fromList (zip (S.toList xs) (repeat $ l + 1)) >< q'


-- | Returns a list of (v, c) pairs that are all connected
components :: CSP -> [Component]
components = map T.flatten . dff

-- | A three-component is a component with at least one (v, c) pair involved
-- in 3 constraints. This is used by triply-constrained colors (section 5.3)
-- where all (v, c) pairs have the same number of constraints as their
-- neighbors, so by that definition all (v, c) pairs in the component will
-- have 3 constraints.
threeComponents :: CSP -> [Component]
threeComponents csp = filter f $ components csp
    where
      f (x:_) = nconstraints x csp == 3
      f []    = error "CSPSearch.threeComponents: found empty component"

-- | Three-component with 4 distinct variables
smallThreeComponents :: CSP -> [Component]
smallThreeComponents = filter ((== 4) . cmptNVars) . threeComponents

-- | Three-components with 5 or more distinct variables
largeThreeComponents :: CSP -> [Component]
largeThreeComponents = filter ((> 4) . cmptNVars) . threeComponents

-- | A witness to a a large three component is a set of 5 (v, c) pairs with
-- 5 distinct vars such that one pair has constraints on three of the others
-- and at least one of the three has a constraint on the 5th. This equation
-- redefines a witness to be the 5th (v, c) pair in such a component.
witnesses :: CSP -> [VarColor]
witnesses csp = mapMaybe (msum . map find_witness) $ largeThreeComponents csp
    where
      find_witness vc
          | length ls < 3     = Nothing
          | cmptNVars l1 < 3  = Nothing
          | any has_v2_con l1 = Nothing
          | otherwise         = Just vc2
          where
            ls           = bfl vc csp
            l1           = map fst $ filter ((1 ==) . snd) ls
            l2           = map fst $ filter ((2 ==) . snd) ls
            vc2@(v2, c2) = head l2
            has_v2_con (v, c) = not $ S.null $ S.filter v2_cons (varColorCons v c csp)
              where
                v2_cons (v', c') = v' == v2 && c' /= c2

-- | A two-component is a component with at least one (v, c) pair involved in
-- 2 constraints. This is used by doubly-constrainted colors (sections 5.4)
-- where all (v, c) pairs have the same number of constraints as their
-- neighbors, so by that definition all (v, c) pairs in the component will
-- have 2 constraints.
twoComponents :: CSP -> [Component]
twoComponents csp = filter f $ components csp
    where
      f (x:_) = nconstraints x csp == 2
      f []    = error "CSPSearch.twoComponents: found empty component"

-- | Two-components with 4 or more distinct (v, c) pairs (not variables).
largeTwoComponents :: CSP -> [Component]
largeTwoComponents = filter ((> 3) . length) . twoComponents

-- | Finds a chain of large two-components that involve five consecutively
-- distinct variables.
fiveVarLargeTwoComponents :: CSP -> [Component]
fiveVarLargeTwoComponents = mapMaybe (\x -> f x S.empty []) . largeTwoComponents
    where
      f _      seen acc | S.size seen == 5 = Just acc
      f []     _    _   = Nothing
      f (x:xs) seen acc
          | ((==) `on` S.size) seen seen' = f xs S.empty []
          | otherwise                     = f xs seen'   acc'
          where
            seen' = S.insert (fst x) seen
            acc'  = x : acc

-- | Finds a string of 4 (v, c) pairs such that the beginning and the end are
-- of the same variable and returns the two (v, c) pairs between these ends.
sandwichedLargeTwoComponents :: CSP -> [Component]
sandwichedLargeTwoComponents = mapMaybe f . largeTwoComponents
    where
      f ((v1,_):rest@(vc2:vc3:(v4,_):_))
          | v1 == v4  = Just [vc2, vc3]
          | otherwise = f rest
      f _ = Nothing

nconstraints :: VarColor -> CSP -> Int
nconstraints (v, c) csp = S.size (varColorCons v c csp)

cmptNVars :: Component -> Int
cmptNVars = length . nub . map fst
