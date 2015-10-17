{-# LANGUAGE FlexibleContexts #-}
module Text.Typeical.Interpreter where 

import Control.Monad.IO.Class;
import Control.Monad;
import System.IO;

import Text.Parsec;
import           Debug.Trace 

import Text.Typeical.Gramma

import Text.Typeical.Parsing
import Text.Typeical.Control
import Text.Typeical.Proof

import Text.Typeical.Readers.BNF 
import Text.Typeical.Readers.SyntaxTree
import Text.Typeical.Readers.InfRule

import Text.Typeical.Writers.BNF
import Text.Typeical.Writers.SyntaxTree
import Text.Typeical.Writers.InfRule

type Interpreter = ParserT String (TypeicalT IO)

stms :: Interpreter ()
stms = void $ stm `manyTill` eof;

seeNext :: (Show s, Monad m) => Int -> ParserT s m ()
seeNext n = do
  s <- getParserState
  let out = take n $ drop 1 (show $ stateInput s)
  traceShowM out

stm :: Interpreter ()
stm = do 
  choice [ addGramma
         , addNewJudgement
         , addNewInfRule
         , patternMatch
         , proveExpr
         , parseExpr
         , parseJudgement
         , restOfLine
         ]

patternMatch :: Interpreter ()
patternMatch = do
  try $ id <$> (string "pattern" .> string "match")
  (gramma, jm) <- lift $ (,) <$> getGramma <*> getJudgements
  skipWs 
  j1 <- skipWs >> judgement gramma jm
  skipWs >> string "with" >> skipWs
  j2 <- skipWs >> judgement gramma jm
  restOfLine
  case match j1 j2 of
    Nothing -> liftIO . putStr $ "Could not match expressions.\n"
    Just match' -> liftIO $ do
      putStr . writeSyntaxExpr $ j1
      putStr " matches with "
      putStr . writeSyntaxExpr $ j2
      putStr "\n"
      print match'

proveExpr :: Interpreter ()
proveExpr = do
    try $ string "prove"
    withDerivation <- option False (string "!" >> return True)
    
    (gramma, jm, rules) <- lift $ (,,) <$> getGramma <*> getJudgements <*> getRules
    tree         <- skipWs >> judgement gramma jm
    restOfLine

    if withDerivation 
      then case buildDerivation rules tree of
          Nothing -> excuse tree
          Just (derv, solution) -> liftIO $ do
            putStr "Solution found to "
            putStr . writeSyntaxExpr $ tree
            putStr ":\n"
            putStr . writeDerivation $ derv
            putStr "\n"
            print solution
      else case prove rules tree of
          Nothing -> excuse tree
          Just solution -> liftIO $ do
            putStr "Solution found to "
            putStr . writeSyntaxExpr $ tree
            putStr ":\n"
            print solution
  where excuse tree = liftIO $ do
          putStr "Could not prove "
          putStr . writeSyntaxExpr $ tree
          putStr ".\n" 

addGramma :: Interpreter ()
addGramma = try $ do 
  gramma' <- lift getGramma
  gramma <- bnf gramma'
  
  lift $ extendGramma gramma
  
  gramma <- lift getGramma
  liftIO $ do putStr "Updated Gramma, gramma is now:\n"
              putStr (writeBNF gramma)

  return ()

printExpr :: SyntaxTree -> IO ()
printExpr tree = do putStr "Parse expression: " 
                    putStr . writeSyntaxExpr $ tree
                    putStr "\n"
                    putStr . writeSyntaxTree $ tree

parseExpr :: Interpreter ()
parseExpr = do 
  s <- try $ char '!' .> symbol -- no way back this is a parse expession
  gramma <- lift getGramma
  ws >> syntaxTree gramma s >>= liftIO . printExpr
  restOfLine

parseJudgement :: Interpreter ()
parseJudgement = do 
  try $ string "!&" -- no way back this is a parse judgement 
  (gramma, jm) <- lift $ (,) <$> getGramma <*> getJudgements
  ws >> judgement gramma jm >>= liftIO . printExpr 
  restOfLine

addNewJudgement :: Interpreter ()
addNewJudgement = do 
  try $ string "judgement"
  symbols <- lift activeSymbols
  judgement <- ws >> concreteTerm symbols
  restOfLine

  lift . addJudgement . Judgement $ judgement
  
  liftIO $ do putStr "Added Judgement: " 
              putStr . writeTerm $ judgement
              putStr "\n"

addNewInfRule :: Interpreter ()
addNewInfRule = do 
  (gramma, jm) <- lift $ (,) <$> getGramma <*> getJudgements
  inf' <- try $ infRule gramma jm
  lift $ addRule inf'
  restOfLine
  liftIO $ do putStr "Found Inference Rule:\n" 
              putStr . writeInfRule $ inf'
              putStr "\n"

runInterpreter :: FilePath -> IO ()
runInterpreter t = do 
    fileContent <- readFile t
    result <- evalT $ runParserT stms () t fileContent
    print result

 
