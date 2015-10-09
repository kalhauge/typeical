{-# LANGUAGE FlexibleInstances #-}

module Text.BNF (bnf, pprintBNF) where

import qualified Data.Map as M;
import           Text.ParserCombinators.Parsec hiding (tokens, token);
import           Data.Bifunctor;
import           Data.Maybe;
import           Data.List;
import           Data.List.Split hiding (oneOf);
import           Control.Monad;


newtype Symbol = Symbol { symbolName :: String 
                        } deriving (Show, Eq, Ord)

data Token = Const String 
           | Ref Symbol
           deriving (Show)

type Expression = [[Token]]
newtype BNF = BNF (M.Map Symbol Expression) deriving (Show)

pprintBNF :: BNF -> ShowS
pprintBNF (BNF m) ss = M.foldrWithKey f ss m
  where 
    f :: Symbol -> Expression -> ShowS 
    f key value = symbol key . 
                  showString " ::= " . 
                  expression value . 
                  showString "\n"

    symbol key = showChar '<' . 
                   showString (symbolName key) .
                   showChar '>'
    
    expression = showIList " | " . map tokens
    
    tokens = showIList " " . map token

    showIList str ts = foldr (.) id newlist
        where newlist = intersperse (showString str) ts
    
    token (Ref (Symbol x)) = showChar '<' . 
                             showString x . 
                             showChar '>'
    token (Const str)      = showChar '"' . 
                             showString (replace "\"" "\\\"" str) . 
                             showChar '"'

-- Parser

type Ambigious e = [Symbol] -> e

instance Show e => Show (Ambigious e) where
    show e = show (e [])

bnf :: Parser BNF
bnf = try $ do 
  exprs <- bnfExpr `endBy1` try (do newline; notFollowedBy nonbreak)
  let symbols = map fst exprs
  void newline <|> eof
  return . BNF . M.fromList $ zip symbols $ mapM snd exprs symbols

bnfExpr :: Parser (Symbol, Ambigious Expression)
bnfExpr = (,) <$> symbol <. string "::=" <.> expression <?> "bnfExpr"

token :: Parser (Ambigious Token)
token = reference <|> constant <|> ambigious <?> "token"

tokens :: Parser (Ambigious [Token])
tokens = sequence <$> helper
  where rest = option [] (try $ skipSpaceOrIndent *> helper)
        helper = (:) <$> token <*> rest

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
          , notFollowedBy letter >> many1 (noneOf "\n\t ")
          ]

symbol :: Parser Symbol
symbol = Symbol <$> (within '<' '>' <|> many1 letter) <?> "symbol"

expression :: Parser (Ambigious Expression)
expression = sequence <$> helper
  where rest = option [] $ (skipSpaceOrIndent *> char '|') .> helper
        helper = (:) <$> tokens <*> rest 

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

-- From Data.String.Utils, MissingH edited to use spiltOn
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = intercalate new . splitOn old
