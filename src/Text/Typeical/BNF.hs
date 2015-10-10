module Text.Typeical.BNF ( BNF()
                         , fromList
                         , asMap
                         
                         , Expression
                         
                         , Token(..)
                         
                         , Symbol(..)
                         ) where

import qualified Data.Map as M;
import Text.ParserCombinators.Parsec hiding (tokens, token)

newtype BNF = BNF { innerMap :: M.Map Symbol Expression}
              deriving (Show)

-- Create a BNF form a list of symbols and expressions
fromList :: [(Symbol, Expression)] -> BNF
fromList = BNF . M.fromList

getExpression :: BNF -> Symbol -> Expression
getExpression bnf s = innerMap bnf M.! s 

-- Return the BNF as a map
asMap :: BNF -> M.Map Symbol Expression
asMap = innerMap

newtype SyntaxTree = SyntaxTree { tree :: (Term, [SyntaxTree]) }

-- Build a parser expr from a BNF
-- genParser :: BNF -> Symbol -> Parser ExprTree
-- genParser bnf sym = choice [genParser]
--     where expr = getExpression bnf sym
-- 
-- genParserTerm :: BNF -> Term -> Parser ExprTree
-- genParserTerm bnf term = try $ do

-- A symbol is a recursive structure
newtype Symbol = Symbol { symbolName :: String 
                        } deriving (Show, Eq, Ord)

data Token = Const String 
           | Ref Symbol
           deriving (Show)

type Term = [Token]
type Expression = [Term]
