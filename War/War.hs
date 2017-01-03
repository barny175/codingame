import System.IO
import Control.Monad
import Control.Applicative
import Data.List

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- input_line <- getLine
    -- let n = read input_line :: Int -- the number of cards for player 1

    -- cards1 <- replicateM n getLine
    let cards1 = ["10H","KD","6C","10S","8S","AD","QS","3D","7H","KH","9D","2D","JC","KS","3S","2S","QC","AC","JH","7D","KC","10D","4C","AS","5D","5S"]
        cards2 = ["2H","9C","8C","4S","5C","AH","JD","QH","7C","5H","4H","6H","6S","QD","9H","10C","4D","JS","6D","3H","8H","3C","7S","9S","8D","2C"]

    hPutStrLn stderr $ show cards1

    -- input_line <- getLine
    -- let m = read input_line :: Int -- the number of cards for player 2

    -- cards2 <- replicateM m getLine
    hPutStrLn stderr $ show cards2

    let (res, turns) = getResult cards1 cards2
        t = if res == "PAT" then "" else " " ++ (show turns)
    putStrLn $ res ++ t
    

getResult _ [] = ("1", 0)
getResult [] _ = ("2", 0)
getResult (x:xs) (y:ys) = case ord of 
    GT -> let (res, turns) = getResult (xs ++ [x,y]) ys in (res, turns + 1) 
    LT -> let (res, turns) = getResult xs (ys ++ [x,y]) in (res, turns + 1) 
    EQ -> let (res, restxs, restys, woncardsx, woncardsy) = war xs ys
              newxs = restxs ++ if res == "1" then x : woncardsx ++ [y] ++ woncardsy else []
              newys = restys ++ if res == "2" then x : woncardsx ++ [y] ++ woncardsy else []
              (rs, turns) = getResult newxs newys
          in if res == "PAT" then ("PAT", 0)
             else (rs, turns + 1)
    where ord = compCards x y  
    
    
compCards c1 c2 = let o1 = cardOrder (take (length c1 - 1) c1)
                      o2 = cardOrder (take (length c2 - 1) c2)
                  in compare o1 o2
                  
cardOrder c = let cards = ["2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"]
              in elemIndex c cards
              
war xs ys
    | length xs < 4 || length ys < 4 = ("PAT", [], [], [], [])
    | otherwise = res
    where c1 = xs !! 3
          c2 = ys !! 3
          ord = compCards c1 c2
          restxs = drop 4 xs
          restys = drop 4 ys
          res = case ord of LT -> ("2", restxs, restys, woncardsx, woncardsy)
                            GT -> ("1", restxs, restys, woncardsx, woncardsy)
                            EQ -> let (rs, rxs, rys, woncardsx2, woncardsy2) = war restxs restys
                                  in (rs, rxs, rys, woncardsx ++ woncardsx2, woncardsy ++ woncardsy2)
          woncardsx = take 4 xs
          woncardsy = take 4 ys