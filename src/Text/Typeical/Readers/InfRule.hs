{-# LANGUAGE FlexibleContexts #-}
module Text.Typeical.Readers.InfRule (infRule, judgement) where

import Text.Typeical.Gramma
import Text.Typeical.Proof
import Text.Typeical.Readers.SyntaxTree

import Text.Typeical.Parsing

judgement :: Stream s m Char => Gramma -> [Judgement] -> ParserT s m SyntaxTree
judgement g js = anyTerm g (map syntax js) <?> "judgement"

infRule :: Stream s m Char => Gramma -> [Judgement] -> ParserT s m InfRule
infRule g js = do 
  prs <- multiLinePremise g js 
  id <- rulerWithId
  con <- judgement g js 
  restOfLine
  return $ InfRule id prs con

oneLinePremise :: Stream s m Char => Gramma -> [Judgement] -> ParserT s m [SyntaxTree]
oneLinePremise g js = do
    j <- judgement g js 
    js <- option [] $ try (skipWs >> oneLinePremise g js)
    return (j:js)
  <?> "one line premise"

multiLinePremise :: Stream s m Char => Gramma -> [Judgement] -> ParserT s m [SyntaxTree]
multiLinePremise g js = do 
  premisLines <- many $ oneLinePremise g js >>~ restOfLine
  return . concat $ premisLines

rulerWithId :: Stream s m Char => ParserT s m String
rulerWithId = do 
    many1 $ char '-'
    skipWs
    id <- choice [ within '(' ')', many1 $ letter <|> oneOf "-" ]
    restOfLine 
    return id
