-- This module contains the elements needed for performing
-- proofs.

module Text.Typeical.Proof (match, prove, substitude, solution, Solution, emptySolution, Match(..), InfRule(..), Judgement(..), buildDerivation, Derivation(..)) where

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

newtype Solution = Solution { scope :: VariableScope } deriving (Eq)

emptySolution :: Solution
emptySolution = Solution M.empty

solution :: [(Variable, SyntaxTree)] -> Solution
solution = Solution . M.fromList

set :: Variable -> SyntaxTree -> Solution -> Solution
set k v s = Solution $ M.insert k v $ scope s 

get :: Variable -> Solution -> Maybe SyntaxTree
get k s = M.lookup k $ scope s 

instance Show Solution where
  showsPrec _ (Solution vs) = 
      showString "{" . list . showString "}"
    where showSingleAss k a = 
            showVariable k . showString " = " . showSyntaxExpr a
          list s = foldr ($) s $ 
            intersperse (showString ", ") $ 
              map (uncurry showSingleAss) $ M.assocs vs

inorder :: Monad m => a -> [a -> m a] -> m a
inorder a = foldl (>>=) (return a) 

sub :: Solution -> SyntaxTree -> SyntaxTree
sub sl (SyntaxTree t subs) = SyntaxTree t $ map (sub sl) subs
sub sl var@(Var v) = fromMaybe var $ get v sl

-- | Apply a solution on a syntax tree, the solution must be a complete
-- mapping for this metod to return the correct result, else it will only
-- give a partial correct solution
apply :: Solution -> SyntaxTree -> SyntaxTree
apply sl (SyntaxTree t subs) = SyntaxTree t $ map (sub sl) subs
apply sl var@(Var v) = fromMaybe var $ get v sl

-- | updateS takes two solutions and adds them to eachother favouring the 
-- new solution
updateS :: Solution -> Solution -> Solution
updateS s new = Solution $ M.union (scope new) (scope s) 

-- | match maybe finds a solution that if applied on the syntax tree
-- will return the second. 
--
-- prop> case match s1 s2 of 
--         Just m -> apply m s1 == s2 
--         Nothing -> True
--
-- prop> case match s1 s1 of
--          Nothing -> False
--          Just m -> apply m s1 == s1
match :: SyntaxTree -> SyntaxTree -> Maybe Solution
match s1 s2 = match' s1 s2 emptySolution
  where match' (SyntaxTree t1 sub1) st sl = case st of 
          SyntaxTree t2 sub2 | t1 == t2 -> 
            inorder sl $ zipWith match' sub1 sub2
          otherwise                     -> Nothing
        match' (Var v) st sl = case get v sl of 
          Nothing -> Just $ set v st sl 
          Just s  -> match' s st emptySolution >> return sl
          -- ^ Ensure that the known solution fits. 

-- Partial match is like match, but does not fail if a variable
-- does not have a direct solution.
partialMatch :: SyntaxTree -> SyntaxTree -> Maybe Solution
partialMatch s1 s2 = match' s1 s2 emptySolution
  where match' (SyntaxTree t1 sub1) st sl = case st of 
          SyntaxTree t2 sub2 | t1 == t2 -> 
            inorder sl $ zipWith match' sub1 sub2
          Var v                         -> return sl 
          otherwise                     -> Nothing
        match' (Var v) st sl = case get v sl of 
          Nothing -> Just $ set v st sl 
          Just s  -> match' s st emptySolution >> return sl

-- | closure tries to find a syntax that both the first and the
-- second can match
closure :: SyntaxTree -> SyntaxTree -> Maybe SyntaxTree
closure s1 s2 = case match s1 s2 of
  Just m1 -> Just s2 
  Nothing -> case match s2 s1 of
      Just m2 -> Just s1
      Nothing -> do 
        m1' <- partialMatch s1 s2
        m2' <- partialMatch s2 s1
        closure (apply m1' s1) (apply m2' s2)
      -- ^ Add more to this case

-- | Solve problems. 
prove :: [InfRule] -> SyntaxTree -> Maybe Solution
prove rules st = snd <$> buildDerivation rules st

buildDerivation :: [InfRule] -> SyntaxTree -> Maybe (Derivation, Solution)
buildDerivation rules st = msum . map (buildOneDerivation rules st) $ rules

buildOneDerivation :: [InfRule] -> SyntaxTree -> InfRule -> Maybe (Derivation, Solution)
buildOneDerivation rules s inf = do
    -- Try to find a closure
    s' <- closure s $ conclusion inf
   
    -- If such exists, create a solution from it.
    mi <- match (conclusion inf) s'

    -- For each of the premisses, prove try to build a derivation in this
    -- new scope updataing the solution if needed.
    (subderv, mi') <- foldM buildOne' ([], mi) (premises inf)

    -- Now apply the solution to the colsure giving us a complete
    -- derivation
    let s'' = apply mi' s' 

    -- Now define the solution to be the match on s'' from s
    mo <- match s s''

    -- Return the created derivation
    return (Derivation inf (reverse subderv) s'', mo)

  where
      -- | Returns maybe a soltution and the derivations in reverse order
      buildOne' :: ([Derivation], Solution) -> SyntaxTree -> Maybe ([Derivation], Solution)
      buildOne' (sd, s) st = do
        (d, new) <- buildDerivation rules (apply s st)
        return (d:sd, updateS s new)
