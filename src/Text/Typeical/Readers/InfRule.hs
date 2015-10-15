{-# LANGUAGE FlexibleContexts #-}
module Text.Typeical.Readers.InfRule where

import Text.Typeical.Gramma
import Text.Typeical.Proof
import Text.Typeical.Readers.SyntaxTree

import Text.Typeical.Parsing

judgement :: Stream s m Char => Gramma -> [Judgement] -> ParserT s m SyntaxTree
judgement g js = anyTerm g (map syntax js)

infRule :: Stream s m Char => Gramma -> [Judgement] -> ParserT s m InfRule
infRule g js = try $ do 
  prs <- multiLinePremise g js
  id <- rulerWithId
  con <- judgement g js
  restOfLine
  return $ InfRule id prs con

oneLinePremise :: Stream s m Char => Gramma -> [Judgement] -> ParserT s m [SyntaxTree]
oneLinePremise g js = judgement g js `sepBy1` try skipWs

multiLinePremise :: Stream s m Char => Gramma -> [Judgement] -> ParserT s m [SyntaxTree]
multiLinePremise g js = concat <$> oneLinePremise g js `endBy` restOfLine

rulerWithId :: Stream s m Char => ParserT s m String
rulerWithId = do 
    many1 $ char '-'
    skipWs
    id <- choice [ within '(' ')', many1 $ letter <|> oneOf "-" ]
    restOfLine 
    return id

