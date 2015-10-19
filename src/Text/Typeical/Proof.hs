-- This module contains the elements needed for performing
-- proofs.

module Text.Typeical.Proof (match, prove, substitude, solution, Solution, emptySolution, Match(..), InfRule(..), Judgement(..), buildDerivation, Derivation(..)) where

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

newtype Solution = Solution { scope :: VariableScope } deriving (Eq)

emptySolution :: Solution
emptySolution = Solution M.empty


solution :: [(Variable, SyntaxTree)] -> Solution
solution = Solution . M.fromList

set :: Variable -> SyntaxTree -> Solution -> Solution
set k v s = Solution $ M.insert k v $ scope s 

get :: Variable -> Solution -> Maybe SyntaxTree
get k s = M.lookup k $ scope s 

newtype Match = Match { solutions :: (Solution, Solution)} deriving (Eq)

leftSolution = fst . solutions 
rightSolution = snd . solutions

newtype CounterExample = CounterExample { failed :: [(Term, Term)]}

instance Show Solution where
  showsPrec _ (Solution vs) = 
      showString "{" . list . showString "}"
    where showSingleAss k a = 
            showVariable k . showString " = " . showSyntaxExpr a
          list s = foldr ($) s $ 
            intersperse (showString ", ") $ 
              map (uncurry showSingleAss) $ M.assocs vs

instance Show Match where
  showsPrec _ (Match (vs1, vs2)) = 
      showString "M(" 
      . shows vs1 
      . showString ", " 
      . shows vs2  
      . showString ")"

inorder :: Monad m => a -> [a -> m a] -> m a
inorder a = foldl (>>=) (return a) 

stable :: (Eq a, Monad m) => (a -> m a) -> a -> m a
stable f a = do a' <- f a 
                if a' == a then return a 
                           else stable f a'

traceS :: (Show a) => a -> a
traceS a = trace (show a) a

isSyntaxTree :: SyntaxTree -> Bool
isSyntaxTree (SyntaxTree _ _) = True
isSyntaxTree _              = False

applyS :: Solution -> Solution -> Solution
applyS o i = Solution $ M.map (substitude i) $ scope o


-- match :: SyntaxTree -> SyntaxTree -> Maybe Match
-- match s1 s2 | trace ("match " ++ writeSyntaxExpr s1 ++ " @ " ++ writeSyntaxExpr s2) True = 
--   do m1 <- match' s1 s2 emptySolution
--      m2 <- match' s2 s1 emptySolution
--      let m = Match (m1, m2)
--      if closure m1 m2 
--        then return m 
--        else do 
--          let s1' = sub m1 s1
--              s2' = sub m2 s2
--          Match(m1', m2') <- match s1' s2'
--          let m1'' = applyS m1 m1'
--              m2'' = applyS m2 m1'
--          return $ Match (applyS m1'' m2'', applyS m2'' m1'')
--   where
--     closure :: Solution -> Solution -> Bool
--     closure s1 s2 = closureS s1 && closureS s2 
-- 
--     closureS :: Solution -> Bool
--     closureS (Solution m) = not $ any isSyntaxTree $ M.elems m 

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

sub :: Solution -> SyntaxTree -> SyntaxTree
sub sl (SyntaxTree t subs) = SyntaxTree t $ map (sub sl) subs
sub sl var@(Var v) = fromMaybe var $ get v sl

substitude :: Solution -> SyntaxTree -> SyntaxTree
substitude sl st = substitude' (variables st) sl st

substitude' :: [Variable] -> Solution -> SyntaxTree -> SyntaxTree
substitude' vs sl (SyntaxTree t subs) = 
    SyntaxTree t $ map (substitude' vs sl) subs
substitude' vs sl var@(Var v) = case get v sl of
    --Just (Var v1) -> var
    Just st -> freeVars vs st
    Nothing -> var

-- | Solve problems. 
prove :: [InfRule] -> SyntaxTree -> Maybe Solution
prove rules st =  snd <$> buildDerivation rules st

buildDerivation :: [InfRule] -> SyntaxTree -> Maybe (Derivation, Solution)
buildDerivation rules st = msum . map (buildOneDerivation rules st) $ rules

buildOneDerivation :: [InfRule] -> SyntaxTree -> InfRule -> Maybe (Derivation, Solution)
buildOneDerivation rules s inf = do
    -- Try to find a closure
    -- traceM ("input (" ++ ruleId inf ++ ") = " ++ writeSyntaxExpr s)
    
    s' <- closure s $ conclusion inf

    -- traceM ("closure (" ++ ruleId inf ++ ") = " ++ writeSyntaxExpr s')
   
    -- If such exists, create a solution from it.
    mi <- match (conclusion inf) s'
    
    -- traceM ("m inner (" ++ ruleId inf ++ ") = " ++ show mi )

    -- For each of the premisses, prove try to build a derivation in this
    -- new scope updataing the solution if needed.
    (subderv, mi') <- foldM buildOne' ([], mi) (premises inf)

    -- traceM ("m inner' (" ++ ruleId inf ++ ") = " ++ show mi')
    -- Now apply the solution to the colsure giving us a complete
    -- derivation
    let s'' = apply mi' s' 

    -- traceM ("output (" ++ ruleId inf ++ ") = " ++ writeSyntaxExpr s'')
    -- Now define the solution to be the match on s'' from s
    mo <- match s s''

    -- traceM ("match (" ++ ruleId inf ++ ") = " ++ show mo)
    -- Return the created derivation
    return (Derivation inf (reverse subderv) s'', mo)

  where
      -- | Returns maybe a soltution and the derivations in reverse order
      buildOne' :: ([Derivation], Solution) -> SyntaxTree -> Maybe ([Derivation], Solution)
      buildOne' (sd, s) st = do
        (d, new) <- buildDerivation rules (apply s st)
        return (d:sd, updateS s new)

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

