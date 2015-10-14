import           System.Environment;
import           Text.Typeical;

data Options = Options { optFilename :: String } deriving (Show)

parseOptions :: [String] -> Options
parseOptions [filename] = Options { optFilename = filename }

main :: IO ()
main = do
    options <- parseOptions <$> getArgs
    runInterpretor (optFilename options)
