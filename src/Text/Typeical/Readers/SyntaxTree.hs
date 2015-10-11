module Text.Typeical.Readers.SyntaxTree (syntaxTree) where

import Text.ParserCombinators.Parsec hiding (tokens, token)

import           Data.Maybe;
import           Data.List;

import           Text.Typeical.BNF;


-- Build a parser expr from a BNF
syntaxTree :: BNF -> Symbol -> Parser SyntaxTree
syntaxTree bnf sym = choice $ map (term bnf) expr
    where expr = getExpression bnf sym

-- 
term :: BNF -> Term -> Parser SyntaxTree
term bnf term = try $ do
  let parsers = map (token bnf) term
  values <- catMaybes <$> sequence (intersperse (spaces >> return Nothing) parsers)
  return $ SyntaxTree (term, values)

token :: BNF -> Token -> Parser (Maybe SyntaxTree)
token bnf (Const str)  = string str >> return Nothing
token bnf (Ref symbol) = Just <$> syntaxTree bnf symbol

