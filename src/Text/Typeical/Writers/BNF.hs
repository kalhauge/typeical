module Text.Typeical.Writers.BNF (showBNF, showTerm, writeBNF, writeTerm, showSymbol) where

import           Text.Typeical.Gramma

import           Data.Bifunctor;
import           Data.Maybe;
import           Data.List;
import           Data.List.Split hiding (oneOf);
import           Control.Monad;

import qualified Data.Map as M;

writeBNF :: Gramma -> String
writeBNF = flip showBNF ""

showBNF :: Gramma -> ShowS
showBNF bnf ss = M.foldrWithKey f ss (asMap bnf)
  where 
    f :: Symbol -> Expression -> ShowS 
    f key value = showSymbol key . 
                  showString " ::= " . 
                  expression value . 
                  showString "\n"
    expression  = showIList " | " . map showTerm 
   
showSymbol :: Symbol -> ShowS
showSymbol key = showChar '<' . showString (symbolName key) . showChar '>'

writeTerm :: Term -> String
writeTerm = flip showTerm ""

showTerm :: Term -> ShowS
showTerm = showIList " " . map showToken
 
showToken :: Token -> ShowS
showToken (Ref (Symbol x)) = showChar '<' . 
                             showString x . 
                             showChar '>'
showToken (Const str)      = showChar '"' . 
                             showString (replace "\"" "\\\"" str) . 
                             showChar '"'

showIList :: String -> [ShowS] -> ShowS
showIList str ts = foldr (.) id $ intersperse (showString str) ts

-- From Data.String.Utils, MissingH edited to use spiltOn
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = intercalate new . splitOn old
