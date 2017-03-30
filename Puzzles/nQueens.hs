import Data.Maybe

n :: Int
n = 4

queens = n

row :: Int
row = 0

ps = [(i, j) | i <- [0..n-1], j <- [0..n-1]]

main = do
    let n = 4
        s = solutions n n 0 [(i, j) | i <- [0..n-1], j <- [0..n-1]]

    putStrLn $ show $ length $ fromJust s

listToListOfLists ps = map (\x -> [x]) ps

solutions :: Int -> Int -> Int -> [(Int, Int)] -> Maybe [[(Int, Int)]]
solutions 0 _ _ _ = Just []
solutions q _ r [] = Nothing
solutions 1 _ _ (p:[]) = Just $ listToListOfLists [p]
solutions queens n row ps
    | row == n - 1 = Just $ listToListOfLists ps
    | otherwise = do
        let qpos = zip [0..n-1] (repeat row)
            addPos p = map (p:)
            solutions' p = addPos p <$> (solutions (queens - 1) n (row + 1) $ filterOut p ps)

        Just $ concat $ catMaybes (map solutions' qpos)


isOnDiagOf (x1, y1) (x2, y2) = abs (x1 - x2) ==  abs (y1 - y2)

filterOut p@(x,y) positions
    | not $ p `elem` positions = []
    | otherwise = filter (not.isReachable) positions
                    where isReachable p@(i, j) = i == x || j == y || p `isOnDiagOf` (x, y)
