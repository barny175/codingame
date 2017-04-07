import System.IO
import Control.Monad
import Text.Printf

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    input_line <- getLine
    let n = read input_line :: Int

    timestamps <- replicateM n $ do
        timestamp <- getLine
        return (timestamp)

    let ts = map timestampToSec timestamps
        result = foldl updateTime (0, 1) ts
    -- hPutStrLn stderr $ show result
    if n == 0 then
        putStrLn "NO GAME"
    else
        putStrLn $ secToTimestamp (fst result)

updateTime (t, p) newTs
    | t > newTs = (t, p)
    | p == 7 = (newTs, p)
    | otherwise = let newStart = start newTs p
                  in (newStart, p + 1)


start time players = let t = fromIntegral time
                         p = fromIntegral players
                         newT = truncate $ t - 256 / ( 2^(p - 1) )
                     in max 0 newT

timestampToSec :: String -> Int
timestampToSec ts
    = let (m,s) = splitAt 1 ts
          min = read m
          sec = read $ drop 1 s
      in min * 60 + sec

secToTimestamp sec = let (m, s) = sec `divMod` 60
                   in printf "%d:%02d" m s
