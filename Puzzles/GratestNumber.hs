import System.IO
import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    input_line <- getLine
    let n = read input_line :: Int
    input <- (map head) . words <$> getLine
    hPutStrLn stderr $ show input


    putStrLn $ greatest input

greatest input
    | minus = '-' : trim res
    | otherwise = trim res
    where
        dot = elem '.' input
        minus = elem '-' input
        numbers = filter (\x -> x /= '-' && x /= '.') input
        sorted = (if minus then id else reverse) $ sort numbers
        res = if dot then if minus then
                          head sorted : '.' : tail sorted
                          else init sorted ++ "." ++ [last sorted]
              else sorted


trim s
    = let dot = '.' `elem` s
          skip = dropWhile (== '0')
          res = if dot then reverse (skip $ reverse s) else s
    --   in if last res == '.' then init res else res
    in res
