import System.IO
import Control.Monad

readInt c = read [c] ::Int

m1 = [[1, 2, 1], [2, 0, 2], [1, 2, 1]]

m2 = [[0, 2, 0],[2, 0, 2],[0, 2, 0]]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    input_line <- getLine
    let n = read input_line :: Int

    -- m1 <- replicateM n $ (map readInt) <$> getLine
    --
    -- m2 <- replicateM n $ (map readInt) <$> getLine

    -- hPutStrLn stderr $ show m1
    -- hPutStrLn stderr $ show m2

    putStrLn $ showMatrix (add m1 m2)

findGT4 m = [(i, j) | i <- [0..2], j <- [0..2], (m !! i) !! j >= 4]

showMatrix m = let showLine = concat . map show
               in unlines $ map showLine m

neighbours i j = filter (\(k, l) -> k >= 0 && k <= 2 && l>= 0 && l <= 2) [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]

scatter m [] = m
scatter m gt4 = let (i, j) = head gt4
                    neighbrMatrices = map (uncurry matrixWithSandAt) (neighbours i j)
                    neighrbsSum = foldr matrixSum (replicate 3 zeros) neighbrMatrices
                    subtractedMat = applyToElemAt m i j (subtract 4)
                in add subtractedMat neighrbsSum

applyToElemAt m i j f = let element = f (m !! i !! j)
                            lnToModify = m !! i
                            modifiedLine = (take j lnToModify) ++ [element] ++ (drop (j+1) lnToModify)
                        in (take i m) ++ [modifiedLine] ++ (drop (i+1) m)

zeros = replicate 3 0

matrixWithSandAt i j
    = let line = (replicate j 0) ++ [1] ++ (replicate (3-j-1) 0)
      in (replicate i zeros) ++ [line] ++ (replicate (3-i-1) zeros)

matrixSum m1 m2 = let lineAdd = zipWith (+)
                  in zipWith lineAdd m1 m2

add m1 m2 = let matSum = matrixSum m1 m2
                gt4 = findGT4 matSum
            in scatter matSum gt4
