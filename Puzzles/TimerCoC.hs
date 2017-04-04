import System.IO
import Data.List

n = 2

timestamps = ["3:55", "3:48"]

main = do
    let ts = map timestampToSec timestamps
        result = foldl updateTime (5 * 60, 1) ts
    -- hPutStrLn stderr $ show result
    putStrLn $ secToTimestamp (fst result)

updateTime (t, p) newTs =
    let newStart = start newTs p
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
                   in (show m) ++ ":" ++ (show s)
