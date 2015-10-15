-- This module contains the elements needed for performing
-- proofs.

module Text.Typeical.Proof where

import Text.Typeical.Writers.SyntaxTree
import Text.Typeical.Gramma
import qualified Data.Map as M
import Data.List

newtype Judgement = Judgement { syntax :: Term 
                              } deriving (Show)

data InfRule = InfRule { ruleId :: String 
                       , premisses :: [SyntaxTree]
                       , conclusion :: SyntaxTree 
                       } deriving (Show)

type VariableScope = M.Map Variable SyntaxTree

newtype Solution = Solution { scopes :: (VariableScope, VariableScope)}

instance Show Solution where
  showsPrec _ (Solution (vs1, vs2)) = 
      showAss vs1 . showString "\n" . showAss vs2
    where showAss vs s = foldr ($) s $ 
            intersperse (showString ", ") $ 
              map (uncurry showSingleAss) $ M.assocs vs
          showSingleAss k a = showVariable k . showString " = " . showSyntaxExpr a

-- | Match tries to pattern match two syntax trees either returning the 
-- first instance where they differ or the variable scopes needed to make
-- the two instances equal
match :: SyntaxTree -> SyntaxTree -> Either (Term, Term) Solution 
match s1 s2 = match' s1 s2 $ Solution (M.empty, M.empty)

match' :: SyntaxTree 
       -> SyntaxTree
       -> Solution 
       -> Either (Term, Term) Solution 
match' (SyntaxTree t1 sub1) (SyntaxTree t2 sub2) vs = 
    if t1 == t2 
      then foldl (>>=) (return vs) $ zipWith match' sub1 sub2
      else Left (t1, t2) 

match' s1@(Var v) s2 sol@(Solution (vs1, vs2)) = 
    case M.lookup v vs1 of
      Nothing -> Right $ Solution (M.insert v s2 vs1, vs2)
      Just s -> match' s s2 sol 

match' s1 s2@(Var v) sol@(Solution (vs1, vs2)) = 
    case M.lookup v vs2 of
      Nothing -> Right $ Solution (vs1, M.insert v s1 vs2)
      Just s -> match' s s2 sol 
