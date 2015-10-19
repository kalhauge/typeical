{-# LANGUAGE FlexibleContexts #-}
module Text.Typeical.Readers.BNF (bnf, symbol, concreteTerm) where

import           Text.Typeical.Parsing

import           Data.List 
import           Debug.Trace 

import           Text.Typeical.Gramma
import           Control.Monad

-- | Ambigious parse result that still need a list of valid symbols
type Ambigious e = [Symbol] -> e

concrete :: [Symbol] -> Ambigious e -> e
concrete s = ($ s)

-- | Parse a gramma using a old possible empty gramma
bnf :: Stream s m Char => Gramma -> ParserT s m Gramma
bnf gramma = do
  exprs <- (bnfExpr gramma <?> "bnf expression") `endBy1` restOfLine
  let ss = map fst exprs
  return . fromList $ zip ss $ mapM snd exprs (symbols gramma `union` ss)

concreteTerm :: Stream s m Char => [Symbol] -> ParserT s m Term
concreteTerm s = concrete s <$> term

bnfExpr :: Stream s m Char => Gramma -> ParserT s m (Symbol, Ambigious Expression)
bnfExpr g = do 
    s <- try (symbol <. string "::=")
    expr <- skipWs >> expression (getExpression g s)
    return (s, expr)

expression :: Stream s m Char => Expression -> ParserT s m (Ambigious Expression)
expression expr = sequence <$> helper
  where rest = option [] $ try ((ws *> char '|') .> helper)
        helper = choice [
            (++) <$> do 
                 try $ string "..." 
                 return $ map const expr
            , (:) <$> term
            ] <*> rest 

term :: Stream s m Char => ParserT s m (Ambigious Term)
term = sequence <$> helper
  where rest = option [] (try $ ws *> helper)
        helper = (:) <$> token <*> rest

token :: Stream s m Char => ParserT s m (Ambigious Token)
token = reference <|> constant <|> ambigious <?> "token"

ambigious :: Stream s m Char => ParserT s m (Ambigious Token)
ambigious = do 
    t <- many1 letter
    let asSymbol = Symbol t
    return $ \s -> 
      if asSymbol `elem` s 
        then Ref asSymbol 
        else Const t

reference :: Stream s m Char => ParserT s m (Ambigious Token)
reference = pure <$> Ref . Symbol <$> within '<' '>' <?> "reference"

constant :: Stream s m Char => ParserT s m (Ambigious Token)
constant = pure <$> Const <$> constants <?> "constant"
  where constants = choice [
            within '\'' '\''
          , within '"' '"'
          , notFollowedBy letter >> many1 (noneOf "|\n\t ")
            -- ^ sings is consideret constants by default
          ]

symbol :: Stream s m Char => ParserT s m Symbol
symbol = Symbol <$> (within '<' '>' <|> wrd) <?> "symbol"


