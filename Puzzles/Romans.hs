import System.IO
import Data.List
import Data.Function

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
        
    input_line <- getLine
    let rom1 = input_line :: String
    input_line <- getLine
    let rom2 = input_line :: String
    putStrLn (araToRom $ (romToAra rom1) + (romToAra rom2))
    
romans = [("IV", 4), ("IX", 9), ("XL", 40), ("XC", 90), ("CD", 400), ("CM", 900), ("I", 1), ("V", 5), ("X", 10), ("L", 50), ("C", 100), ("D", 500), ("M", 1000)]

romToAra [] = 0
romToAra rom = let Just curr = find (\r -> take (length (fst r)) rom == fst r) romans
               in snd curr + romToAra (drop (length $ fst curr) rom) 

araToRom 0 = ""               
araToRom ara 
    = let sorted = sortBy (flip compare `on` snd) romans
          Just r = find ((<=ara) . snd) sorted
          dm = ara `divMod` (snd r)
      in (concat $ replicate (fst dm) (fst r)) ++ (araToRom (snd dm))  