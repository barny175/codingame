import System.IO
import Control.Monad

-- lake = [[242,64,172,208,222,106,101],[158,211,225,221,132,71,104],[6,74,45,184,33,170,109],[216,58,125,123,3,53,151],[216,255,175,202,64,92,154],[30,197,254,188,152,224,153],[29,39,2,34,113,47,218],[145,216,71,106,18,196,228]]
-- lake = [[211,225,221,132,71,104],[74,45,184,33,170,109],[58,125,123,3,53,151],[255,175,202,64,92,154],[197,254,188,152,224,153],[39,2,34,113,47,218],[216,71,106,18,196,228]]
-- lake = [[45,184,33,170,109],[125,123,3,53,151],[175,202,64,92,154],[254,188,152,224,153],[2,34,113,47,218],[71,106,18,196,228]]
lake = [[224,153],[47,218],[196,228]]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    input_line <- getLine
    let input = words input_line
    let w = read (input!!0) :: Int
    let h = read (input!!1) :: Int

    lake <- replicateM h $ do
        input_line <- getLine
        return (map (\s -> read s :: Int) $ words input_line)

    let (h, v) = maxFood lake
    putStrLn $ show (max (maximum h) (maximum v))

maxFood as
    | length as == 1 = (head as, [sum $ head as])
    | length (head as)  == 1 = ([sum $ concat as], map head as)
    | otherwise = let (horiz, vert) = maxFood $ map tail (tail as)
                      horizLine = head as
                      bestHoriz i = maximum $ map (\j -> horiz !! (i-1 + j - 1) + sum (take j $ drop i horizLine)) [1..length horizLine - i]
                      bestVert i = maximum $ map (\j -> vert !! (i-1 + j - 1) + sum (take j $ drop i $ map head as))  [1..length as - i]
                      bestCommon = head horizLine + (max (bestVert 1) (bestHoriz 1))
                  in (bestCommon : map bestHoriz [1..length horiz], bestCommon : map bestVert [1..length as - 1])
