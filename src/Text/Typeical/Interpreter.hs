module Text.Typeical.Interpreter where 

import Control.Monad.IO.Class;
import Control.Monad;
import System.IO;

import Text.Parsec;

import Text.Typeical.Parsing
import Text.Typeical.Control

import Text.Typeical.Readers.BNF 
import Text.Typeical.Readers.SyntaxTree

import Text.Typeical.Writers.BNF
import Text.Typeical.Writers.SyntaxTree

type Interpreter = ParserT String (TypeicalT IO)

stms :: Interpreter ()
stms = void $ stm `manyTill` eof;

stm :: Interpreter ()
stm = choice [ addGramma
             , parseExpr
             , restOfLine 
             ] <?> "statement"

addGramma :: Interpreter ()
addGramma = try $ do 
  gramma <- bnf
  lift $ extendGramma gramma
  
  gramma <- lift getGramma
  liftIO $ do putStr "Updated Gramma, gramma is now:\n"
              putStr (writeBNF gramma)

  return ()

parseExpr :: Interpreter ()
parseExpr = try $ do 
  s <- char '!' .> symbol
  gramma <- lift getGramma
  tree <- ws >> syntaxTree gramma s
  liftIO $ do putStr "Parse expression: " 
              putStr . writeSyntaxExpr $ tree
              putStr "\n"
              putStr . writeSyntaxTree $ tree


runInterpreter :: FilePath -> IO ()
runInterpreter t = do 
    fileContent <- readFile t
    result <- evalT $ runParserT stms () t fileContent
    print result

 
