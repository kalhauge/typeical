-- This module holds helpers functions and the other things to help build
-- parsers.
{-# LANGUAGE FlexibleContexts #-}
module Text.Typeical.Parsing ( ParserT
                             , (.>)
                             , (<.)
                             , (<.>)
                             , (<?>)
                             , (<|>)
                             , Stream
                             , allowIndent
                             , char
                             , choice
                             , endBy1
                             , endOfLine
                             , letter
                             , many
                             , many1
                             , noneOf
                             , oneOf
                             , notFollowedBy
                             , option
                             , spaces
                             , string
                             , try
                             , within
                             , wrd
                             , ws
                             , line
                             , comment
                             , emptyLine
                             , restOfLine
                             , skipWs
                             , nat
                             ) 
                             where

import           Text.Parsec;
import           Data.List;
import           Control.Monad;

type ParserT s m = ParsecT s () m

within :: Stream s m Char => Char -> Char -> ParserT s m String 
within begin end = char begin *> many1 (noneOf [end]) <* char end

-- | Allow indent, takes a parser which does not accept linebreaks and
-- returns a new parser that can handle linebreaks, as long as it
-- is indented. 
allowIndent :: (Stream String m Char) => Int -> ParserT String m a -> ParserT String m a
allowIndent min parser = do 
    s <- line                       -- parse the initial line
    rest <- many $ parseIndent min  -- parse possible indents
    parseFromString parser $ unwords (s:rest)

-- | Parse a single indented line, with a minimum idention. The minimum
-- indention is removed from the returning string.
parseIndent :: Stream s m Char => Int -> ParserT s m String
parseIndent min = try $ count min ws >> line

-- | Parses a line, e.g. all charactors until a endOfLine
line :: Stream s m Char => ParserT s m String
line = manyTill anyChar $ try endOfLine

-- | Parse the content of a string, borrowed from Pandoc
parseFromString :: Monad m => ParserT String m a -> String -> ParserT String m a
parseFromString parser str = keepContext $ do
  setInput str
  result <- parser 
  spaces >> eof -- Parse until end of string
  return result

-- | Keeps context of the parser, especially the postion and the input
keepContext :: Monad m => ParserT s m a -> ParserT s m a
keepContext parser = do 
  oldPos <- getPosition
  oldInput <- getInput
  retsult <- parser 
  setInput oldInput
  setPosition oldPos
  return retsult

-- | Parse a word 
wrd :: Stream s m Char => ParserT s m String
wrd = many1 letter

-- | Parse a nonbreaking whitespace charactor
ws :: Stream s m Char => ParserT s m Char
ws = oneOf " \t"

-- | Skip alot of nonbreaking whitespaces
skipWs :: Stream s m Char => ParserT s m ()
skipWs = void $ many ws 

-- | Parses all whitespaces, or comments until a newline
restOfLine :: Stream s m Char => ParserT s m ()
restOfLine = void $ emptyLine <|> comment

-- | Parse an empty line
emptyLine :: Stream s m Char => ParserT s m String
emptyLine = try $ ws `manyTill` endOfLine 

-- | Parses a comment
comment :: Stream s m Char => ParserT s m String
comment = try $ char ';' >> line

-- | Natual number
nat :: Stream s m Char => ParserT s m Int
nat = read <$> many1 digit

-- | inline compinator for skiping spaces
infixl 4 <.>, .>, <.
(<.>) :: Stream s m Char => ParserT s m (a -> b) -> ParserT s m a -> ParserT s m b
p <.> s = p <*> (skipWs *> s)

(.>) :: Stream s m Char => ParserT s m a -> ParserT s m b -> ParserT s m b
p .> s = p >> skipWs >> s

(<.) :: Stream s m Char => ParserT s m a -> ParserT s m b -> ParserT s m a
p <. s = p >>= \x -> skipWs >> s >> return x
