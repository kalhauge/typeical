{-# LANGUAGE FlexibleContexts #-}
module Text.Typeical.Readers.BNF (bnf, symbol, concreteTerm) where

import           Text.Typeical.Parsing

import           Data.List 

import           Text.Typeical.Gramma
import           Control.Monad

-- | Ambigious parse result that still need a list of valid symbols
type Ambigious e = [Symbol] -> e

concrete :: [Symbol] -> Ambigious e -> e
concrete s = ($ s)

bnf :: Stream s m Char => [Symbol] -> ParserT s m Gramma
bnf symbols' = try $ do 
  exprs <- bnfExpr `endBy1` try endOfLine
  let symbols = map fst exprs
  return . fromList $ zip symbols $ mapM snd exprs (symbols' `union` symbols)

concreteTerm :: Stream s m Char => [Symbol] -> ParserT s m Term
concreteTerm s = concrete s <$> term

bnfExpr :: Stream s m Char => ParserT s m (Symbol, Ambigious Expression)
bnfExpr = (,) <$> symbol <. string "::=" <.> expression <?> "bnf expression"

expression :: Stream s m Char => ParserT s m (Ambigious Expression)
expression = sequence <$> helper
  where rest = option [] $ (ws *> char '|') .> helper
        helper = (:) <$> term <*> rest 

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


