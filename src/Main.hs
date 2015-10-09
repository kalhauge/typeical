import           System.Environment;

import           Text.ParserCombinators.Parsec;
import           Text.BNF;

data Options = Options { optFilename :: String } deriving (Show)

parseOptions :: [String] -> Options
parseOptions [filename] = Options { optFilename = filename }

main :: IO ()
main = do
    options <- parseOptions <$> getArgs
    result <- parseFromFile bnf (optFilename options)
    case result of 
        Left parseError -> print parseError
        Right bnf -> putStr $ pprintBNF bnf ""
