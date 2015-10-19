module Text.Typeical.Gramma ( Gramma()
                            , fromList
                            , empty
                            , asMap
                            , getExpression
                            , extend
                            , symbols
                            
                            , Expression
                            
                            , Term

                            , Token(..)
                            
                            , Symbol(..)
                            
                            , SyntaxTree(..)
                            , Variable(..)
                            ) where

import qualified Data.Map as M;

newtype Gramma = Gramma { innerMap :: M.Map Symbol Expression
                        } deriving (Show, Eq)

-- | Create a Gramma form a list of symbols and expressions
fromList :: [(Symbol, Expression)] -> Gramma
fromList = Gramma . M.fromList

-- | Extend a gramma, using a new gramma, overriding old rules
extend :: Gramma -> Gramma -> Gramma
extend g1 g2 = Gramma $ asMap g2 `M.union` asMap g1

-- | Get a expression from a gramma
getExpression :: Gramma -> Symbol -> Expression
getExpression bnf s = innerMap bnf M.! s 

-- | Return the Gramma as a map
asMap :: Gramma -> M.Map Symbol Expression
asMap = innerMap

-- | Finds all the symbols in the gramma
symbols :: Gramma -> [Symbol]
symbols = M.keys . asMap

-- | Empty gramma
empty :: Gramma
empty = Gramma M.empty

-- A symbol is a recursive structure
newtype Symbol = Symbol { symbolName :: String 
                        } deriving (Show, Eq, Ord)

type Expression = [Term]

type Term = [Token]

data Token = Const String 
           | Ref Symbol
           deriving (Show, Eq)

data Variable = Variable { varSymbol :: Symbol 
                         , varMajor :: Int 
                         , varMinor :: Int 
                         } deriving (Show, Eq, Ord)

data SyntaxTree = SyntaxTree Term [SyntaxTree]
                | Var Variable
                deriving (Show, Eq)


