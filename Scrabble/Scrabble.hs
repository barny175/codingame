import System.IO
import Control.Monad
import Data.List
import Data.Maybe

pointTable = [("eaionrtlsu", 1),
    ("dg", 2),
    ("bcmp", 3),
    ("fhvwy", 4),
    ("k", 5),
    ("jx", 8),
    ("qz", 10)]

point c = let csearch s = c `elem` s
              Just p = snd <$> find (csearch.fst) pointTable
          in p
          
score s = sum $ map point s

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    input_line <- getLine
    let n = read input_line :: Int
    
    replicateM n $ do
        w <- getLine
        return ()
    letters <- getLine
    
    hPutStrLn stderr $ show $ score "banjo"
    
    putStrLn "invalid word"