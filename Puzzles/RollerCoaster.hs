import System.IO
import Control.Monad
import Control.Monad.State
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

selectGroups [] _ = ([], [])    
selectGroups groups@(g:gs) places 
    | places == 0 || g > places= ([], groups)
    | g <= places = (g:sg, rest)
    where (sg,rest) = selectGroups gs (places - g)

ride places rides groups people 
    | rides == 0 = 0
    | people < places = people + (ride places (rides - 1) groups people)
    | otherwise = dirhams
    where (sg, rest) = selectGroups groups places
          dirhams = sum sg + (ride places (rides - 1) (rest ++ sg) people)