module Main where

import Lib
import System.IO
import Control.Monad
import Data.Time.Clock

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    input_line <- getLine
    let input = words input_line
    let l = read (input!!0) :: Int
    let c = read (input!!1) :: Int
    let n = read (input!!2) :: Int

    groups <- replicateM n $ do
        input_line <- getLine
        let pi = read input_line :: Int
        return (pi)

    -- hPutStrLn stderr $ "places: " ++ (show l) ++ " people: " ++ (show $ sum groups)
    let people = sum groups
    ct <- getCurrentTime
    hPutStrLn stderr $ show ct
    putStrLn $ show $ ride l c groups people
    ct <- getCurrentTime
    hPutStrLn stderr $ show ct
