-- This module contains the elements needed for performing
-- proofs.

module Text.Typeical.Proof (match, prove, substitude, Solution, Match, InfRule(..), Judgement(..)) where

import Debug.Trace
import Control.Monad
import Text.Typeical.Writers.SyntaxTree
import Text.Typeical.Gramma
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Data.List

newtype Judgement = Judgement { syntax :: Term 
                              } deriving (Show)

data InfRule = InfRule { ruleId :: String 
                       , premises :: [SyntaxTree]
                       , conclusion :: SyntaxTree 
                       } deriving (Show)

type VariableScope = M.Map Variable SyntaxTree

newtype Solution = Solution { scope :: VariableScope }

emptySolution :: Solution
emptySolution = Solution M.empty

set :: Variable -> SyntaxTree -> Solution -> Solution
set k v s = Solution $ M.insert k v $ scope s 

get :: Variable -> Solution -> Maybe SyntaxTree
get k s = M.lookup k $ scope s 

newtype Match = Match { solutions :: (Solution, Solution)}

leftSolution = fst . solutions 
rightSolution = snd . solutions

newtype CounterExample = CounterExample { failed :: [(Term, Term)]}

instance Show Solution where
  showsPrec _ (Solution vs) s = foldr ($) s $ 
      intersperse (showString ", ") $ 
        map (uncurry showSingleAss) $ M.assocs vs
    where showSingleAss k a = showVariable k . showString " = " . showSyntaxExpr a

instance Show Match where
  showsPrec _ (Match (vs1, vs2)) = 
      shows vs1 . showString "\n" . shows vs2

-- | Match tries to pattern match two syntax trees either returning the 
-- first instance where they differ or the variable scopes needed to make
-- the two instances equal
match :: SyntaxTree -> SyntaxTree -> Maybe Match
match s1 s2 = match' s1 s2 $ Match (emptySolution, emptySolution) 

match' :: SyntaxTree 
       -> SyntaxTree
       -> Match 
       -> Maybe Match 
match' (SyntaxTree t1 sub1) (SyntaxTree t2 sub2) vs | t1 == t2 = 
  foldl (>>=) (return vs) $ zipWith match' sub1 sub2

match' s1@(SyntaxTree _ _) (Var v) match@(Match (vs1, vs2)) = 
  case get v vs2 of
    Nothing -> Just $ Match (vs1, set v s1 vs2)
    Just s -> match' s1 s match 

match' s1@(Var v1) s2 match@(Match (vs1, vs2)) = 
  case get v1 vs1 of
    Nothing -> case s2 of
    -- ^ v1 is a free variable
      Var v2 -> case get v2 vs2 of
        Nothing -> Just $ Match (set v1 s2 vs1, set v2 s1 vs2)
        -- ^ Two free variables
        Just s -> Just $ Match (set v1 s2 vs1, vs2)
        -- ^ Actually has a value
      otherwise -> Just $ Match (set v1 s2 vs1, vs2)
    Just s -> match' s s2 match 

match' _ _ _ = Nothing

substitude :: Solution -> SyntaxTree -> SyntaxTree
substitude s (SyntaxTree t subs) = 
    SyntaxTree t $ map (substitude s) subs
substitude s var@(Var v) = fromMaybe var $ get v s

-- | Solve problems. 
prove :: [InfRule] -> SyntaxTree -> Maybe Solution
prove rules st = msum $ map (proveOne rules st) rules

proveOne :: [InfRule] -> SyntaxTree -> InfRule -> Maybe Solution
proveOne rules st inf = do 
    -- Try to match the conclusion
    Match (outer, inner) <- match st $ conclusion inf

    -- traceM $ showSyntaxExpr st ""

    -- traceM $ "inner  (" ++ ruleId inf ++ ") " ++ show inner
    -- Try to prove the first permis in the inference rules using the inner
    -- solution. Use this updated solution to prove the next premise. 
    inner' <- foldM proveOne' inner $ premises inf
    
    -- traceM $ "inner' (" ++ ruleId inf ++ ") " ++ show inner'

    -- Hopefully we have a solution that is able to prove all premises, now
    -- we have to check is the outer solution, depends on any instances
    -- from the inner solution. If so replace them.

    -- TODO: this might not take variable overlap into account.
    let outer' = Solution $ M.map (substitude inner') $ scope outer

    -- traceM $ "outer  (" ++ ruleId inf ++ ") " ++ show outer
    -- traceM $ "outer' (" ++ ruleId inf ++ ") " ++ show outer'

    return outer'
    
  where 
    -- | ProveOne' tries to prove syntax tree piece, and updates the
    -- solution.
    proveOne' :: Solution -> SyntaxTree -> Maybe Solution
    proveOne' s st = do
      newSolution <- prove rules (substitude s st) 
      return . Solution $ M.union (scope s) (scope newSolution)
      -- ^ Merge with the old solutions.. because..
