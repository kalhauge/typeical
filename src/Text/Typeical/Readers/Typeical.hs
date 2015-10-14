module Text.Typeical.Readers.Typeical where

import           Text.Parsec hiding (tokens, token);
import           Text.Typeical;

import           Text.Typeical.Readers.BNF (bnf) ;
-- import           Text.Typeical.Readers.SyntaxTree (syntaxTree);

type Parser = ParsecT String () Typeical

statement :: Parser ()
statement = try $ do
  choice [
      bnfStatement
    ] <?> "statement"

bnfStatement :: Parser ()
bnfStatement = try $ do
  bnf' <- bnf
  return ()
