import System.IO
import Control.Monad
import Data.List

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    input <- words <$> getLine
    let width = read (input!!0) :: Int
    let height = read (input!!1) :: Int

    mapa <- replicateM height $ getLine

    let counts = map (length . filter (=='#')) (transpose mapa)
        res = transpose $ map (col height) counts

    putStrLn $ concat $ intersperse "\n" res

col h n = replicate (h - n) '.' ++ replicate n '#'
