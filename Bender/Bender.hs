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
        dirs = fst $ unzip $ directions
        isLoop = LOOP == last dirs
    showProgression pos $ take 180 directions
    if isLoop then
        putStrLn "LOOP"
    else
        putStrLn $ unlines $ take 180 $ map show dirs

showProgression _ [] = return ()
showProgression pos (dir:dirs) = do
    let rows = snd dir
        repl = replaceAt rows pos '*'
        newPos = move pos $ fst dir
    hPutStrLn stderr $ unlines repl
    showProgression newPos dirs

data Bender = Bender { currentDir :: DIRECTION, pos :: (Int, Int), breaker :: Bool, inverted :: Bool, visited :: [(Bender, [String])] }
                deriving Show

instance Eq Bender where
    (==) a b = currentDir a == currentDir b && pos a == pos b && breaker a == breaker b && inverted a == inverted b

findSymbol rows symbol =
    let findAt = elemIndices symbol
        posY = findIndices (not . null . findAt) rows
        map' y = map (\x -> (x, y)) $ findAt $ rows !! y
        poss = concat $ map map' posY
    in poss

direction :: [String] -> Bender -> [(DIRECTION, [String])]
direction rows bender =
    let (x,y) = pos bender
        symbol' = symbol rows x y
        currDir = currentDir bender
    in nextMove rows bender

nextMove :: [String] -> Bender -> [(DIRECTION, [String])]
nextMove rows bender =
        let dir = currentDir bender
            directions = dir : delete dir ((if inverted bender then reverse else id ) [SOUTH .. WEST])
            visited' = (bender { visited = [] }, rows) : visited bender
            bender' = bender { visited = visited' }
            moves = map (tryMove rows bender') directions
            isLoop = elem (bender,rows) $ visited bender
            Just mv = find isJust moves
        in if isLoop then [(LOOP, rows)] else fromJust mv

symbol rows x y= (rows !! y) !! x

tryMove :: [String] -> Bender -> DIRECTION -> Maybe [(DIRECTION, [String])]
tryMove rows bender dir = let (newPosX, newPosY) = move (pos bender) dir
                              symbol' = symbol rows newPosX newPosY
                              withoutObstacle = replaceAt rows (newPosX, newPosY) ' '
                              isBreaker = breaker bender
                              isInverted = inverted bender
                              tPos = head $ findSymbol rows 'T' \\ [(newPosX, newPosY)]
                              bender' = bender { pos = (newPosX, newPosY)}
                          in case symbol' of
                              '@' -> Just [(LOOP, rows)]
                              '$' -> Just [(dir, rows)]
                              '#' -> Nothing
                              'I' -> Just $ (dir, rows) : nextMove rows bender' { inverted = not isInverted }
                              'X' -> if isBreaker
                                     then Just $ (dir, rows) : nextMove withoutObstacle bender'
                                     else Nothing
                              'T' -> Just $ (dir, rows) : nextMove rows bender' { pos = tPos }
                              'B' -> Just $ (dir, rows) : nextMove rows bender' { breaker = (not isBreaker) }
                              'S' -> Just $ (dir, rows) : nextMove rows bender' { currentDir = SOUTH}
                              'W' -> Just $ (dir, rows) : nextMove rows bender' { currentDir = WEST}
                              'E' -> Just $ (dir, rows) : nextMove rows bender' { currentDir = EAST}
                              'N' -> Just $ (dir, rows) : nextMove rows bender' { currentDir = NORTH}
                              _ -> Just $ (dir, rows) : nextMove rows bender' { currentDir = dir }

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
