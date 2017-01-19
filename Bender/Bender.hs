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
    
    let (posX, posY) = head $ findSymbol rows '@'
        bender = newBender SOUTH (posX, posY) False False
    direction rows bender
    return ()
    
data Bender = Bender { currentDir :: DIRECTION, pos :: (Int, Int), breaker :: Bool, inverted :: Bool }
                deriving Show

findSymbol rows symbol = 
    let findAt = elemIndices symbol
        posY = findIndices (not . null . findAt) rows
        map' y = map (\x -> (x, y)) $ findAt $ rows !! y
        poss = concat $ map map' posY
    in poss
    
newBender currentDir pos breaker inverted = Bender currentDir pos breaker inverted

direction :: [String] -> Bender -> IO ()
direction rows bender = do 
    -- hPutStrLn stderr $ show bender
    let (x,y) = pos bender
        symbol' = symbol rows x y
        tPos = head $ findSymbol rows 'T' \\ [(x,y)]
        b = if symbol' == 'T' 
            then bender {pos = tPos} 
            else bender
    nextMove rows b

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
                              'I' -> Just $ printAndGo rows dir $ newBender dir (newPosX, newPosY) isBreaker (not isInverted)
                              'X' -> if isBreaker
                                     then Just $ printAndGo withoutObstacle dir $ newBender dir (newPosX, newPosY) isBreaker isInverted
                                     else Nothing
                              'B' -> Just $ printAndGo rows dir $ newBender dir (newPosX, newPosY) (not isBreaker) isInverted
                              'S' -> Just $ printAndGo rows dir $ newBender SOUTH (newPosX, newPosY) isBreaker isInverted
                              'W' -> Just $ printAndGo rows dir $ newBender WEST (newPosX, newPosY) isBreaker isInverted
                              'E' -> Just $ printAndGo rows dir $ newBender EAST (newPosX, newPosY) isBreaker isInverted
                              'N' -> Just $ printAndGo rows dir $ newBender NORTH (newPosX, newPosY) isBreaker isInverted
                              _ -> Just $ printAndGo rows dir $ newBender dir (newPosX, newPosY) isBreaker isInverted

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