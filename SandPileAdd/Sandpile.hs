import System.IO
import Control.Monad
import Data.List

readInt c = read [c] ::Int

-- m1 = [[1, 2, 1], [2, 0, 2], [1, 2, 1]]
-- m2 = [[0, 2, 0],[2, 0, 2],[0, 2, 0]]

m1 = [[3, 1, 1, 3], [1, 2, 2, 1], [1, 2, 2, 1], [3, 1, 1, 3]]
m2 = [[1, 0, 0, 2], [0, 3, 0, 0], [0, 0, 3, 0], [2, 0, 0, 1]]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    -- input_line <- getLine
    -- let n = read input_line :: Int

    -- m1 <- replicateM n $ (map readInt) <$> getLine
    --
    -- m2 <- replicateM n $ (map readInt) <$> getLine

    -- hPutStrLn stderr $ show m1
    -- hPutStrLn stderr $ show m2

    putStrLn $ showMatrix (add m1 m2)

findGT4 m = [(i, j) | i <- [0..n], j <- [0..n], (m !! i) !! j >= 4]
            where n = length m - 1

showMatrix m = let showLine = concat . map show
               in concat $ intersperse "\n" $ map showLine m

neighbours i j n = filter (\(k, l) -> k >= 0 && k <= (n-1) && l>= 0 && l <= (n-1)) [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]

scatter m [] = m
scatter m gt4 = let n = length m
                    (i, j) = head gt4
                    neighbrMatrices = map (uncurry (matrixWithSandAt n)) (neighbours i j n)
                    neighrbsSum = foldr matrixSum (replicate n (zeros n)) neighbrMatrices
                    subtractedMat = applyToElemAt m i j (subtract 4)
                in add subtractedMat neighrbsSum

applyToElemAt m i j f = let element = f (m !! i !! j)
                            lnToModify = m !! i
                            modifiedLine = (take j lnToModify) ++ [element] ++ (drop (j+1) lnToModify)
                        in (take i m) ++ [modifiedLine] ++ (drop (i+1) m)

zeros n = replicate n 0

matrixWithSandAt n i j
    = let zeroM = replicate n (replicate n 0)
      in applyToElemAt zeroM i j (\_ -> 1)

matrixSum m1 m2 = let lineAdd = zipWith (+)
                  in zipWith lineAdd m1 m2

add m1 m2 = let matSum = matrixSum m1 m2
                gt4 = findGT4 matSum
            in scatter matSum gt4
