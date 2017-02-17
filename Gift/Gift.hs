import System.IO
import Control.Monad
import Control.Applicative
import Data.List

readInt i = read i :: Int

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- input_line <- getLine
    -- let n = readInt input_line
    -- price <- readInt <$> getLine

    let price = 100
        budgets = [40, 40, 40]
        -- budgets = [100, 1, 60]

    hPutStrLn stderr $ show price
    -- budgets <- replicateM n $ readInt <$> getLine

    hPutStrLn stderr $ show budgets

    let suma = sum budgets
        grouped = reverse . group . sort $ budgets

    -- hPutStrLn stderr $ show grouped
    if suma < price then
        putStrLn "IMPOSSIBLE"
    else do
        putStrLn $ unlines.map show $ sort $ divide grouped (suma - price)


subtractOverPrice bs overPrice minPrice
    = let avg = overPrice `div` (length bs)
          toSubtract = min avg (head bs - minPrice)
          adjusted = map (subtract toSubtract) bs
          len = length bs
          rest = overPrice - (toSubtract * len)
      in (adjusted, rest)

applyRest :: [Int] -> Int -> [Int]
applyRest xs rest = let (ar,newRest) = foldl (\(bs, remains) x -> (if remains > 0 then (x-1):bs else bs ++ [x], remains - 1)) ([], rest) xs
                    in if newRest > 0 then applyRest ar newRest else ar

divide :: [[Int]] -> Int -> [Int]
divide (b:[]) overPrice = let (result, rest) = subtractOverPrice b overPrice 0
                              len = length b
                              ar = applyRest result rest
                          in if rest < len then ar else result
divide (b:bs:[]) overPrice = let (result,rest) = subtractOverPrice b overPrice (head bs)
                                 ar = applyRest (result ++ bs) rest
                                 len = length b
                             in if rest > 0 then ar else result ++ bs
divide (b:bs:bbs) overPrice = let (result,rest) = subtractOverPrice b overPrice (head bs)
                                  ar = applyRest result rest
                                  len = length b
                              in if rest < len
                                 then ar
                                 else divide ((result++bs) : bbs) rest
