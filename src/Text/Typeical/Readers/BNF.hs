module Text.Typeical.Readers.BNF (bnf) where

import           Text.ParserCombinators.Parsec hiding (tokens, token);

import           Text.Typeical.BNF;
import           Control.Monad;

type Ambigious e = [Symbol] -> e

bnf :: Parser BNF
bnf = try $ do 
  exprs <- bnfExpr `endBy1` try (do newline; notFollowedBy nonbreak)
  let symbols = map fst exprs
  void newline <|> eof
  return . fromList $ zip symbols $ mapM snd exprs symbols

bnfExpr :: Parser (Symbol, Ambigious Expression)
bnfExpr = (,) <$> symbol <. string "::=" <.> expression <?> "bnfExpr"

term :: Parser (Ambigious Term)
term = sequence <$> helper
  where rest = option [] (try $ skipSpaceOrIndent *> helper)
        helper = (:) <$> token <*> rest

token :: Parser (Ambigious Token)
token = reference <|> constant <|> ambigious <?> "token"

ambigious :: Parser (Ambigious Token)
ambigious = do 
    t <- many1 letter
    let asSymbol = Symbol t
    return $ \s -> 
      if asSymbol `elem` s 
        then Ref asSymbol 
        else Const t

reference :: Parser (Ambigious Token)
reference = pure <$> Ref . Symbol <$> within '<' '>' <?> "reference"

constant :: Parser (Ambigious Token)
constant = pure <$> Const <$> constants <?> "constant"
  where constants = choice [
            within '\'' '\''
          , within '"' '"'
          , notFollowedBy letter >> many1 (noneOf "|\n\t ")
          ]

symbol :: Parser Symbol
symbol = Symbol <$> (within '<' '>' <|> many1 letter) <?> "symbol"

expression :: Parser (Ambigious Expression)
expression = sequence <$> helper
  where rest = option [] $ (skipSpaceOrIndent *> char '|') .> helper
        helper = (:) <$> term <*> rest 

within :: Char -> Char -> Parser String 
within begin end = char begin *> many1 (noneOf [end]) <* char end

skipSpaceOrIndent :: Parser ()
skipSpaceOrIndent = skipMany nonbreak >> optional (try indent) <?> "whitespace or indent"

nonbreak :: Parser Char
nonbreak = oneOf "\t\f \r"

indent :: Parser String
indent = newline >> many1 nonbreak 

infixl 4 <.>, .>, <.
(<.>) :: Parser (a -> b) -> Parser a -> Parser b
p <.> s = p <*> (skipSpaceOrIndent *> s)

(.>) :: Parser a -> Parser b -> Parser b
p .> s = p >> skipSpaceOrIndent >> s

(<.) :: Parser a -> Parser b -> Parser a
p <. s = do 
    x <- p 
    skipSpaceOrIndent 
    s
    return x
