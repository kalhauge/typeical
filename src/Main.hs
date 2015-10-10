import           System.Environment;

import           Text.ParserCombinators.Parsec;

import           Text.Typeical.BNF;
import           Text.Typeical.Readers.BNF;
import           Text.Typeical.Writers.BNF;

data Options = Options { optFilename :: String } deriving (Show)

parseOptions :: [String] -> Options
parseOptions [filename] = Options { optFilename = filename }

main :: IO ()
main = do
    options <- parseOptions <$> getArgs
    result <- parseFromFile bnf (optFilename options)
    case result of 
        Left parseError -> print parseError
        Right bnf -> putStr $ writeBNF bnf
