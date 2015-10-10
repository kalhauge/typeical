module Text.Typeical.Writers.BNF (showBNF, writeBNF) where

import           Text.Typeical.BNF

import           Data.Bifunctor;
import           Data.Maybe;
import           Data.List;
import           Data.List.Split hiding (oneOf);
import           Control.Monad;

import qualified Data.Map as M;

writeBNF :: BNF -> String
writeBNF = flip showBNF ""

showBNF :: BNF -> ShowS
showBNF bnf ss = M.foldrWithKey f ss (asMap bnf)
  where 
    f :: Symbol -> Expression -> ShowS 
    f key value = symbol key . 
                  showString " ::= " . 
                  expression value . 
                  showString "\n"

    symbol key = showChar '<' . 
                   showString (symbolName key) .
                   showChar '>'
    
    expression = showIList " | " . map tokens
    
    tokens = showIList " " . map token

    showIList str ts = foldr (.) id newlist
        where newlist = intersperse (showString str) ts
    
    token (Ref (Symbol x)) = showChar '<' . 
                             showString x . 
                             showChar '>'
    token (Const str)      = showChar '"' . 
                             showString (replace "\"" "\\\"" str) . 
                             showChar '"'

-- From Data.String.Utils, MissingH edited to use spiltOn
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = intercalate new . splitOn old
