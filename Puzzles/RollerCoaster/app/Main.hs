module Main where

import Lib
import System.IO
import Control.Monad
import Data.Time.Clock
import Control.Monad.Writer.Lazy
import Data.Array

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
    -- putStrLn $ show $ ride l c groups people
    -- let (res, log) = runWriter $ rideW l c (listArray (0, length groups - 1) groups) 0
    putStrLn $ show $ ride l c groups
    -- putStrLn log
    ct <- getCurrentTime
    hPutStrLn stderr $ show ct
