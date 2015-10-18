-- This module contains the elements needed for performing
-- proofs.

module Text.Typeical.Proof (match, prove, substitude, Solution, Match, InfRule(..), Judgement(..), buildDerivation, Derivation(..)) where

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

data Derivation = Derivation { rule :: InfRule
                             , subderv :: [Derivation]
                             , fact :: SyntaxTree
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
  showsPrec _ (Solution vs) = 
      showString "Solution = [" . list . showString "]"
    where showSingleAss k a = 
            showVariable k . showString " = " . showSyntaxExpr a
          list s = foldr ($) s $ 
            intersperse (showString ", ") $ 
              map (uncurry showSingleAss) $ M.assocs vs

instance Show Match where
  showsPrec _ (Match (vs1, vs2)) = 
      shows vs1 . showString "\n" . shows vs2

match :: SyntaxTree -> SyntaxTree -> Maybe Match
match s1 s2 = do
    m1 <- match' s1 (freeVars (variables s1) s2) emptySolution
    m2 <- match' s2 (freeVars (variables s2) s1) emptySolution 
    return $ Match (m1, m2)

-- | match returns the solution nesseary to make one syntax tree match
-- an other.
match' :: SyntaxTree -> SyntaxTree -> Solution -> Maybe Solution
--match' s1 s2 sl | trace ("match' " ++ writeSyntaxExpr s1 ++ " " ++ writeSyntaxExpr s2 ++ " " ++ show sl) False = undefined
match' (SyntaxTree t1 sub1) st sl =  case st of 
    Var v                         -> Just sl
    SyntaxTree t2 sub2 | t1 == t2 -> 
      foldl (>>=) (return sl) $ zipWith match' sub1 sub2
    otherwise                     -> Nothing
match' (Var v) st sl = case get v sl of 
    Nothing -> Just (set v st sl)
    Just s  -> set v st <$> match' st s sl

preserve :: (a -> b) -> a -> (a, b)
preserve f a = (a, f a)

rename :: [Variable] -> Variable -> Variable
-- rename vs v | trace ("rename " ++ show (map (`showVariable` "") vs) ++ " " ++ showVariable v "") False = undefined
rename (v:vs) v' | varSymbol v == varSymbol v' && v >= v' = 
    rename vs $ Variable (varSymbol v) (varMajor v + 1) 0
rename (v:vs) v' = rename vs v'
rename [] v' = v'

freeVars :: [Variable] -> SyntaxTree -> SyntaxTree
freeVars vs st = substitude sol st
  where sol        = Solution . M.fromList $ map helper vs
        helper var = (var, Var $ rename vars var)
        vars       = variables st

variables :: SyntaxTree -> [Variable]
variables (Var v) = [v]
variables (SyntaxTree _ subs) = concatMap variables subs

substitude :: Solution -> SyntaxTree -> SyntaxTree
substitude s (SyntaxTree t subs) = 
    SyntaxTree t $ map (substitude s) subs
substitude s var@(Var v) = fromMaybe var $ get v s


-- | Solve problems. 
prove :: [InfRule] -> SyntaxTree -> Maybe Solution
prove rules st =  snd <$> buildDerivation rules st

buildDerivation :: [InfRule] -> SyntaxTree -> Maybe (Derivation, Solution)
buildDerivation rules st = msum . map (buildOneDerivation rules st) $ rules

buildOneDerivation :: [InfRule] -> SyntaxTree -> InfRule -> Maybe (Derivation, Solution)
buildOneDerivation rules st inf = do
    -- Try to match the conclusion
    Match (outer, inner) <- match st $ conclusion inf
    
    traceM $ "in -> " ++ writeSyntaxExpr st
    
    traceM $ "inner  (" ++ ruleId inf ++ ") " ++ show inner
    traceM $ "outer  (" ++ ruleId inf ++ ") " ++ show outer

    -- Try to prove the first permis in the inference rules using the inner
    -- solution. Use this updated solution to prove the next premise. 
    (subderv, inner'') <- foldM buildOne' ([], inner) (premises inf)

    let inner' = updateSolution inner inner''
    
    traceM $ "inner' (" ++ ruleId inf ++ ") " ++ show inner'
    
    -- Hopefully we have a solution that is able to prove all premises, now
    -- we have to check is the outer solution, depends on any instances
    -- from the inner solution. If so replace them.
    let outer' = Solution $ M.map (substitude inner') $ scope outer

    traceM $ "outer' (" ++ ruleId inf ++ ") " ++ show outer'
    
    let outerExpr = substitude outer' st

    traceM $ "out -> " ++ writeSyntaxExpr outerExpr

    return (Derivation inf (reverse subderv) outerExpr, outer')

  where
      -- | Returns maybe a soltution and the derivations in reverse order
      buildOne' :: ([Derivation], Solution) -> SyntaxTree -> Maybe ([Derivation], Solution)
      buildOne' (sd, s) st = do
        (d, new) <- buildDerivation rules (substitude s st)
        return (d:sd, updateSolution s new)
      updateSolution s new = Solution $ M.union (scope s) (scope new)

renaming :: Match -> Match
renaming (Match (Solution vs1,Solution vs2)) = 
    Match ( Solution $ M.union vs1' vs1''
          , Solution $ M.union vs2' vs2'' )
    where 
      sharedVars = M.keys vs2 ++ M.keys vs1 
      ((sharedVars', vs1''), vs2') = M.mapAccum renaming' (sharedVars, vs1) vs2 
      ((_, vs2''), vs1') = M.mapAccum renaming' (sharedVars', vs2) vs1
      -- | renaming', figures out if any of the variables represented
      -- in syntax tree is free. If they are rename them and repace the
      -- new variable in 
      renaming' :: ([Variable],  VariableScope)
                 -- ^ Variables we know is used
                 -- ^ the scope we want to add stuff to
                -> SyntaxTree 
                 -- ^ the syntax tree we work with
                -> (([Variable], VariableScope), SyntaxTree)
      renaming' (names, vs) s = foldl update ((names, vs), s) transforms
        where transforms = map (preserve $ rename names) $ variables s 

      update :: (([Variable], VariableScope), SyntaxTree) 
             -> (Variable, Variable) 
             -> (([Variable], VariableScope), SyntaxTree)
      update ((vs, m), st) (from, to) = (
          (to:vs, M.insert from (Var to) m), 
          substitude (Solution $ M.singleton from (Var to)) st
        )

