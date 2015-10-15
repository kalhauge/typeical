{-# LANGUAGE FlexibleContexts #-}
module Text.Typeical.Readers.SyntaxTree (syntaxTree, anyTerm) where

import           Text.Typeical.Parsing;
import           Text.Typeical.Readers.BNF (symbol);
import           Data.Maybe;
import           Data.List;

import           Control.Monad;

import           Text.Typeical.Gramma;

-- Build a parser expr from a Gramma
syntaxTree :: Stream s m Char => Gramma -> Symbol -> ParserT s m SyntaxTree
syntaxTree bnf sym = do
    t <- anyTerm bnf simple
    option t $ oneOfRecTerms t lrec
    where (lrec, simple) = partition (isLeftRec sym) $ getExpression bnf sym
          oneOfRecTerms t = choice . map (recTerm bnf t)

-- | Figures out if a method is left recursive
isLeftRec :: Symbol -> Term -> Bool
isLeftRec sym (Ref s:_) = s == sym
isLeftRec _ _           = False

-- | parses any term
anyTerm :: Stream s m Char => Gramma -> [Term] -> ParserT s m SyntaxTree
anyTerm bnf = choice . map (term bnf)

-- | Parses a term
term :: Stream s m Char => Gramma -> Term -> ParserT s m SyntaxTree
term bnf term = try $ do
    values <- catMaybes <$> sep parsers spaces 
    return $ SyntaxTree term values
  where parsers = map (token bnf) term

-- | Parse a left recursive term with the term already parsed
recTerm :: Stream s m Char => Gramma -> SyntaxTree -> Term -> ParserT s m SyntaxTree
recTerm bnf t term = try $ do
    skipWs 
    values <- catMaybes <$> sep parsers spaces 
    return $ SyntaxTree term (t:values)
  where parsers = map (token bnf) (tail term)

token :: Stream s m Char => Gramma -> Token -> ParserT s m (Maybe SyntaxTree)
token bnf (Const str)  = string str >> return Nothing <?> str
token bnf (Ref sym) = Just <$> choice [ 
                syntaxTree bnf sym
              , (do s <- try $ do 
                      s <- symbol 
                      guard $ sym == s
                      return s
                    major <- option (-1) nat
                    minor <- option 0 $ length <$> many (char '\'')
                    return . Var $ Variable s major minor
                ) <?> "variable"
              ]

sep :: Stream s m Char => [ParserT s m a] -> ParserT s m b -> ParserT s m [a]
sep [p]      s = (: []) <$> p
sep (p:rest) s = (:) <$> p <.> sep rest s
