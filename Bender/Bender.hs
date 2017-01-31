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
        isLoop = LOOP == last directions
    showProgression pos (take 180 directions) rows
    if isLoop then
        putStrLn "LOOP"
    else
        putStrLn $ unlines $ take 180 $ map show directions

showProgression _ [] _ = return ()
showProgression pos (dir:dirs) rows = do
    let repl = replaceAt rows pos '*'
        newPos = move pos dir
    hPutStrLn stderr $ unlines repl
    showProgression newPos dirs rows

data Bender = Bender { currentDir :: DIRECTION, pos :: (Int, Int), breaker :: Bool, inverted :: Bool, visited :: [Bender] }
                deriving Show

instance Eq Bender where
    (==) a b = currentDir a == currentDir b && pos a == pos b && breaker a == breaker b && inverted a == inverted b


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
        visited' = bender : visited bender
        b = bender {visited = visited'}
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
                              withoutObstacle = replaceAt rows (newPosX, newPosY) ' '
                              isBreaker = breaker bender
                              isInverted = inverted bender
                              tPos = head $ findSymbol rows 'T' \\ [(newPosX, newPosY)]
                              visited' = bender { visited = [] } : visited bender
                              bender' = bender { pos = (newPosX, newPosY), visited = visited' }
                              isLoop = elem bender {currentDir = dir, pos = (newPosX,newPosY)} $ visited bender
                          in if isLoop then Just [LOOP]
                             else case symbol' of
                              '@' -> Just [LOOP]
                              '$' -> Just [dir]
                              '#' -> Nothing
                              'I' -> Just $ dir : nextMove rows bender' { inverted = not isInverted }
                              'X' -> if isBreaker
                                     then Just $ dir : nextMove withoutObstacle bender'
                                     else Nothing
                              'T' -> Just $ dir : nextMove rows bender' { pos = tPos }
                              'B' -> Just $ dir : nextMove rows bender' { breaker = (not isBreaker) }
                              'S' -> Just $ dir : nextMove rows bender' { currentDir = SOUTH}
                              'W' -> Just $ dir : nextMove rows bender' { currentDir = WEST}
                              'E' -> Just $ dir : nextMove rows bender' { currentDir = EAST}
                              'N' -> Just $ dir : nextMove rows bender' { currentDir = NORTH}
                              _ -> Just $ dir : nextMove rows bender' { currentDir = dir }

replaceAt :: [String] -> (Int, Int) -> Char -> [String]
replaceAt rows (posX, posY) c = let replace index replacement list = take index list ++ [replacement] ++ ( drop (index + 1) list)
                                    newRow = replace posX c (rows !! posY)
                                in replace posY newRow rows

move (posX, posY) SOUTH = (posX, posY + 1)
move (posX, posY) EAST = (posX + 1, posY)
move (posX, posY) NORTH = (posX, posY - 1)
move (posX, posY) WEST = (posX - 1, posY)


data DIRECTION =  SOUTH | EAST | NORTH | WEST | LOOP
                    deriving (Show, Enum, Eq)
