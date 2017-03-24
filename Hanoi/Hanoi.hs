import System.IO
import Control.Monad
import Data.Array
import Data.List

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    input_line <- getLine
    let n = read input_line :: Int
    input_line <- getLine
    let t = read input_line :: Int
    
    let disks = (array (0, 2) $ zip [0..2] (repeat [])) // [(0, [1..n])]
    
    hPutStrLn stderr $ show $ move disks 0 2 1
    
    let moves = move disks 0 2 n
        turns = length moves
    putStr $ showState (moves !! (t-1)) n
    putStrLn $ show $ turns

intermed:: Int -> Int -> Int
intermed f t = head $ filter (\x -> x /= f && x /= t) [0..2] 

move ds from to 1 = let fromDs = ds ! from
                        toDs = ds ! to
                    in [ds // [(from, drop 1 fromDs),(to, take 1 fromDs ++ toDs)]]
move ds from to n = let interm = intermed from to
                        first = move ds from interm (n-1)
                        second = move (last first) from to 1
                        third = move (last second) interm to (n-1)
                    in  first ++ second ++ third

disk n c s r = let spaces = (replicate (n - r) ' ')
                   center = [s]
                   chars = replicate r c
               in spaces ++ chars ++ center ++ chars ++ spaces

showState :: Array Int [Int] -> Int -> String
showState disks n =
    let width = 2 * n + 1
        axe ds = (replicate (n - length ds) (disk n ' ' '|' 0)) ++ (map (disk n '#' '#') ds)  
        disks' = map (\i -> axe $ disks  ! i) [0..2]
        removeSpaceAtEnd = map (dropWhileEnd (== ' ')) $ foldr (\d1 d2 -> zipWith (\x y -> x ++ " " ++ y) d1 d2) (replicate n []) disks' 
    in unlines removeSpaceAtEnd   
