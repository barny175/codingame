import System.IO
import Control.Monad
import Data.List
import Data.Maybe

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    handle <- openFile "loop.txt" ReadMode

    input_line <- hGetLine handle
    let input = words input_line
    let l = read (input!!0) :: Int
    let c = read (input!!1) :: Int

    rows <- replicateM l $ hGetLine handle

    hPutStrLn stderr $ unlines rows

    let pos = head $ findSymbol rows '@'
        bender = Bender SOUTH pos False False []
    direction rows bender
    return ()

data Bender = Bender { currentDir :: DIRECTION, pos :: (Int, Int), breaker :: Bool, inverted :: Bool, visited :: [(Int, Int , DIRECTION)] }
                deriving Show

findSymbol rows symbol =
    let findAt = elemIndices symbol
        posY = findIndices (not . null . findAt) rows
        map' y = map (\x -> (x, y)) $ findAt $ rows !! y
        poss = concat $ map map' posY
    in poss

direction :: [String] -> Bender -> IO ()
direction rows bender = do
    -- hPutStrLn stderr $ show bender
    let (x,y) = pos bender
        symbol' = symbol rows x y
        currDir = currentDir bender
        tPos = head $ findSymbol rows 'T' \\ [(x,y)]
        visited' = (x, y, currDir) : visited bender
        b = if symbol' == 'T'
            then bender {pos = tPos, visited = (fst tPos, snd tPos, currDir) : visited'}
            else bender {visited = visited'}

    if isLoop bender
    then putStrLn "LOOP"
    else nextMove rows b

isLoop bender
    | isJust v = True
    | otherwise = False
    where (x, y) = pos bender
          v = find (== (x, y, currentDir bender)) (visited bender)

nextMove :: [String] -> Bender -> IO ()
nextMove rows bender = do
        let dir = currentDir bender
            directions = dir : delete dir ((if inverted bender then reverse else id ) [SOUTH .. WEST])
            moves = map (tryMove rows bender) directions
            Just mv = find isJust moves

        fromJust mv

symbol rows x y= (rows !! y) !! x

tryMove :: [String] -> Bender -> DIRECTION -> Maybe (IO ())
tryMove rows bender dir = let (newPosX, newPosY) = move (pos bender) dir
                              symbol' = symbol rows newPosX newPosY
                              withoutObstacle = removeX rows (newPosX, newPosY)
                              isBreaker = breaker bender
                              isInverted = inverted bender
                          in case symbol' of
                              '@' -> Just $ putStrLn "LOOP"
                              '$' -> Just $ do putStrLn (show dir); return ()
                              '#' -> Nothing
                              'I' -> Just $ printAndGo rows dir $ bender { pos = (newPosX, newPosY) , inverted = not isInverted }
                              'X' -> if isBreaker
                                     then Just $ printAndGo withoutObstacle dir $ bender { pos = (newPosX, newPosY) }
                                     else Nothing
                              'B' -> Just $ printAndGo rows dir $ bender { pos = (newPosX, newPosY), breaker = (not isBreaker) }
                              'S' -> Just $ printAndGo rows dir $ bender { currentDir = SOUTH, pos = (newPosX, newPosY) }
                              'W' -> Just $ printAndGo rows dir $ bender { currentDir = WEST, pos =  (newPosX, newPosY) }
                              'E' -> Just $ printAndGo rows dir $ bender { currentDir = EAST, pos =  (newPosX, newPosY) }
                              'N' -> Just $ printAndGo rows dir $ bender { currentDir = NORTH, pos = (newPosX, newPosY) }
                              _ -> Just $ printAndGo rows dir $ bender { currentDir = dir, pos = (newPosX, newPosY) }

removeX :: [String] -> (Int, Int) -> [String]
removeX rows (posX, posY) = let replace index replacement list = take index list ++ [replacement] ++ ( drop (index + 1) list)
                                newRow = replace posX ' ' (rows !! posY)
                            in replace posY newRow rows

printAndGo :: [String] -> DIRECTION -> Bender -> IO ()
printAndGo rows dir bender = do
    -- hPutStrLn stderr $ show bender
    putStrLn $ show dir
    direction rows bender
    return ()

move (posX, posY) SOUTH = (posX, posY + 1)
move (posX, posY) EAST = (posX + 1, posY)
move (posX, posY) NORTH = (posX, posY - 1)
move (posX, posY) WEST = (posX - 1, posY)


data DIRECTION =  SOUTH | EAST | NORTH | WEST
                    deriving (Show, Enum, Eq)
