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

    if suma < price then
        putStrLn "IMPOSSIBLE"
    else do
        putStrLn $ unlines.map show $ divide grouped (suma - price)


subtractOverPrice bs overPrice minPrice
    = let avg = overPrice `div` (length bs)
          toSubtract = min avg (head bs - minPrice)
          adjusted = map (subtract toSubtract) bs
          len = length bs
          rest = overPrice - (toSubtract * len)
          (applyRest, _) = foldl (\(bs, remains) x -> (if remains > 0 then (x-1):bs else bs ++ [x], remains - 1)) ([], rest) adjusted
      in if rest < len
         then (applyRest, 0) else (adjusted, rest)

divide :: [[Int]] -> Int -> [Int]
divide (b:[]) overPrice = let (result,_) = subtractOverPrice b overPrice 0
                          in result
