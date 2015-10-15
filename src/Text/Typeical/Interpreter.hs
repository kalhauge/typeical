module Text.Typeical.Interpreter where 

import Control.Monad.IO.Class;
import Control.Monad;
import System.IO;

import Text.Parsec;

import Text.Typeical.Gramma

import Text.Typeical.Parsing
import Text.Typeical.Control
import Text.Typeical.Proof

import Text.Typeical.Readers.BNF 
import Text.Typeical.Readers.SyntaxTree
import Text.Typeical.Readers.InfRule

import Text.Typeical.Writers.BNF
import Text.Typeical.Writers.SyntaxTree

type Interpreter = ParserT String (TypeicalT IO)

stms :: Interpreter ()
stms = void $ stm `manyTill` eof;

stm :: Interpreter ()
stm = choice [ addGramma
             , addNewJudgement
             , addNewInfRule
             , parseExpr
             , parseJudgement
             , restOfLine 
             ] <?> "statement"

addGramma :: Interpreter ()
addGramma = try $ do 
  gramma' <- lift getGramma
  gramma <- bnf gramma'
  
  lift $ extendGramma gramma
  
  gramma <- lift getGramma
  liftIO $ do putStr "Updated Gramma, gramma is now:\n"
              putStr (writeBNF gramma)

  return ()

parseExpr :: Interpreter ()
parseExpr = do 
  s <- try $ char '!' .> symbol -- no way back this is a parse expession
  gramma <- lift getGramma
  tree <- ws >> syntaxTree gramma s
  liftIO $ do putStr "Parse expression: " 
              putStr . writeSyntaxExpr $ tree
              putStr "\n"
              -- putStr . writeSyntaxTree $ tree

parseJudgement :: Interpreter ()
parseJudgement = do 
  try $ string "!&" -- no way back this is a parse judgement 
  (gramma, jm) <- lift $ (,) <$> getGramma <*> getJudgements
  tree <- ws >> judgement gramma jm
  liftIO $ do putStr "Parse expression: " 
              putStr . writeSyntaxExpr $ tree
              putStr "\n"
              -- putStr . writeSyntaxTree $ tree

addNewJudgement :: Interpreter ()
addNewJudgement = do 
  try $ string "judgement"
  symbols <- lift activeSymbols
  judgement <- ws >> concreteTerm symbols

  lift . addJudgement . Judgement $ judgement
  
  liftIO $ do putStr "Added Judgement: " 
              putStr . writeTerm $ judgement
              putStr "\n"

addNewInfRule :: Interpreter ()
addNewInfRule = do 
  (gramma, jm) <- lift $ (,) <$> getGramma <*> getJudgements
  inf' <- infRule gramma jm
  liftIO $ do putStr "Found Inference Rule:\n" 
              print inf'
              putStr "\n"

runInterpreter :: FilePath -> IO ()
runInterpreter t = do 
    fileContent <- readFile t
    result <- evalT $ runParserT stms () t fileContent
    print result

 
