{-# LANGUAGE FlexibleContexts #-}
module Text.Typeical.Readers.SyntaxTree (syntaxTree) where

import           Text.Typeical.Parsing;
import           Data.Maybe;
import           Data.List;

import           Text.Typeical.Gramma;


-- Build a parser expr from a Gramma
syntaxTree :: Stream s m Char => Gramma -> Symbol -> ParserT s m SyntaxTree
syntaxTree bnf sym = choice $ map (term bnf) expr
    where expr = getExpression bnf sym

-- 
term :: Stream s m Char => Gramma -> Term -> ParserT s m SyntaxTree
term bnf term = try $ do
  let parsers = map (token bnf) term
  values <- catMaybes <$> sequence (intersperse (spaces >> return Nothing) parsers)
  return $ SyntaxTree (term, values)

token :: Stream s m Char => Gramma -> Token -> ParserT s m (Maybe SyntaxTree)
token bnf (Const str)  = string str >> return Nothing
token bnf (Ref symbol) = Just <$> syntaxTree bnf symbol

