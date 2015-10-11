module Text.Typeical.BNF ( BNF()
                         , fromList
                         , asMap
                         , getExpression
                         
                         , Expression
                         
                         , Term

                         , Token(..)
                         
                         , Symbol(..)
                         
                         , SyntaxTree(..)
                         ) where

import qualified Data.Map as M;

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

-- A symbol is a recursive structure
newtype Symbol = Symbol { symbolName :: String 
                        } deriving (Show, Eq, Ord)

data Token = Const String 
           | Ref Symbol
           deriving (Show)

type Term = [Token]
type Expression = [Term]

newtype SyntaxTree = SyntaxTree { tree :: (Term, [SyntaxTree]) } deriving (Show)


