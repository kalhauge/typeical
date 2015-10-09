{-# LANGUAGE FlexibleInstances #-}

module Text.BNF (bnf, pprintBNF) where

import qualified Data.Map as M;
import           Text.ParserCombinators.Parsec hiding (tokens, token);
import           Data.Bifunctor;
import           Data.Maybe;

newtype Symbol = Symbol { symbolName :: String 
                        } deriving (Show, Eq, Ord)

data Token = Constant String 
           | Reference Symbol
           deriving (Show)

type Expression = [[Token]]
newtype BNF = BNF (M.Map Symbol Expression) deriving (Show)

bnf :: Parser BNF
bnf = do 
  exprs <- bnfExpr `endBy1` (char '.' *> newline)
  let symbols = map fst exprs
  return . BNF . M.fromList $ map (second ($ symbols)) exprs

pprintBNF :: BNF -> ShowS
pprintBNF (BNF m) ss = M.foldrWithKey f ss m
  where f :: Symbol -> Expression -> ShowS 
        f key value = pprintSymbol key 
            .  (" ::= " ++) 
            .  pprintExpression value
            .  ("\n" ++)

pprintSymbol :: Symbol -> ShowS
pprintSymbol key s = ("<" ++ symbolName key ++ ">") ++ s

pprintExpression :: Expression -> ShowS
pprintExpression (e:e2:es) = pprintTokens e . (" | " ++) . pprintExpression (e2:es)
pprintExpression [e]       = pprintTokens e

pprintTokens (t:t2:ts) = pprintToken t . (" "++) . pprintTokens (t2:ts)
pprintTokens [t]       = pprintToken t

pprintToken (Reference (Symbol x)) s = ("<" ++ x ++ ">") ++ s
pprintToken (Constant str) s           = ("\"" ++ str ++ "\"") ++ s
    

bnfExpr :: Parser (Symbol, Ambigious Expression)
bnfExpr = do
    s <- symbol
    spaces 
    string "::="
    spaces
    expr <- expression
    return (s, expr)

type Ambigious e = [Symbol] -> e

token :: Parser (Ambigious Token)
token = reference <|> constant <|> ambigious

ambigiousTokens :: Parser [Ambigious Token]
ambigiousTokens = (:) <$> token <*> rest
  where rest = option [] (try (spaces *> ambigiousTokens))

tokens :: Parser (Ambigious [Token])
tokens = do ts <- ambigiousTokens
            return $ \s -> map ($ s) ts

ambigious :: Parser (Ambigious Token)
ambigious = do 
    t <- many1 letter
    let asSymbol = Symbol t
    return $ \s -> 
      if asSymbol `elem` s 
        then Reference asSymbol 
        else Constant t

reference :: Parser (Ambigious Token)
reference = do t <- parser; return $ pure t
    where parser = Reference . Symbol <$> within '<' '>'

constant :: Parser (Ambigious Token)
constant = do t <- parser; return $ pure t 
    where parser = Constant <$> (within '\'' '\'' <|> within '"' '"')

within :: Char -> Char -> Parser String 
within begin end = char begin *> many1 (noneOf [end]) <* char end

symbol :: Parser Symbol
symbol = Symbol <$> (within '<' '>' <|> many1 letter)

ambigiousExpressions :: Parser [Ambigious [Token]]
ambigiousExpressions = (:) <$> tokens <*> rest
  where rest = option [] $ spaces *> char '|' *> spaces *> ambigiousExpressions

expression :: Parser (Ambigious Expression)
expression = do expr <- ambigiousExpressions
                return $ \s -> map ($ s) expr
