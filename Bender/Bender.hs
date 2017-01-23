import System.IO
import Control.Monad
import Data.List
import Data.Maybe

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    input_line <- getLine
    let input = words input_line
    let l = read (input!!0) :: Int
    let c = read (input!!1) :: Int

    rows <- replicateM l getLine

    hPutStrLn stderr $ unlines rows

    let pos = head $ findSymbol rows '@'
        bender = Bender SOUTH pos False False []
        directions = direction rows bender
    putStrLn $ unlines $ map show directions

data Bender = Bender { currentDir :: DIRECTION, pos :: (Int, Int), breaker :: Bool, inverted :: Bool, visited :: [(Int, Int , DIRECTION)] }
                deriving Show

findSymbol rows symbol =
    let findAt = elemIndices symbol
        posY = findIndices (not . null . findAt) rows
        map' y = map (\x -> (x, y)) $ findAt $ rows !! y
        poss = concat $ map map' posY
    in poss

direction :: [String] -> Bender -> [DIRECTION]
direction rows bender =
    let (x,y) = pos bender
        symbol' = symbol rows x y
        currDir = currentDir bender
        tPos = head $ findSymbol rows 'T' \\ [(x,y)]
        visited' = (x, y, currDir) : visited bender
        b = if symbol' == 'T'
            then bender {pos = tPos, visited = (fst tPos, snd tPos, currDir) : visited'}
            else bender {visited = visited'}
    in nextMove rows b

nextMove :: [String] -> Bender -> [DIRECTION]
nextMove rows bender =
        let dir = currentDir bender
            directions = dir : delete dir ((if inverted bender then reverse else id ) [SOUTH .. WEST])
            moves = map (tryMove rows bender) directions
            Just mv = find isJust moves
        in fromJust mv

symbol rows x y= (rows !! y) !! x

tryMove :: [String] -> Bender -> DIRECTION -> Maybe [DIRECTION]
tryMove rows bender dir = let (newPosX, newPosY) = move (pos bender) dir
                              symbol' = symbol rows newPosX newPosY
                              withoutObstacle = removeX rows (newPosX, newPosY)
                              isBreaker = breaker bender
                              isInverted = inverted bender
                          in case symbol' of
                              '@' -> Just [LOOP]
                              '$' -> Just [dir]
                              '#' -> Nothing
                              'I' -> Just $ dir : nextMove rows bender { pos = (newPosX, newPosY) , inverted = not isInverted }
                              'X' -> if isBreaker
                                     then Just $ dir : nextMove rows bender { pos = (newPosX, newPosY) }
                                     else Nothing
                              'B' -> Just $ dir : nextMove rows bender { pos = (newPosX, newPosY), breaker = (not isBreaker) }
                              'S' -> Just $ dir : nextMove rows bender { currentDir = SOUTH, pos = (newPosX, newPosY) }
                              'W' -> Just $ dir : nextMove rows bender { currentDir = WEST, pos =  (newPosX, newPosY) }
                              'E' -> Just $ dir : nextMove rows bender { currentDir = EAST, pos =  (newPosX, newPosY) }
                              'N' -> Just $ dir : nextMove rows bender { currentDir = NORTH, pos = (newPosX, newPosY) }
                              _ -> Just $ dir : nextMove rows bender { currentDir = dir, pos = (newPosX, newPosY) }

removeX :: [String] -> (Int, Int) -> [String]
removeX rows (posX, posY) = let replace index replacement list = take index list ++ [replacement] ++ ( drop (index + 1) list)
                                newRow = replace posX ' ' (rows !! posY)
                            in replace posY newRow rows

move (posX, posY) SOUTH = (posX, posY + 1)
move (posX, posY) EAST = (posX + 1, posY)
move (posX, posY) NORTH = (posX, posY - 1)
move (posX, posY) WEST = (posX - 1, posY)


data DIRECTION =  SOUTH | EAST | NORTH | WEST | LOOP
                    deriving (Show, Enum, Eq)
