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
    
    let findAt = elemIndex '@'
        Just posY = findIndex (isJust . findAt) rows
        Just posX = findAt $ rows !! posY
        bender = Bender SOUTH (posX, posY) False
    direction rows bender
    return ()
    
data Bender = Bender { currentDir :: DIRECTION, pos :: (Int, Int), breaker :: Bool }
                deriving Show

direction :: [String] -> Bender -> IO ()
direction rows bender = do 
    -- hPutStrLn stderr $ show bender
    
    nextMove rows bender

nextMove :: [String] -> Bender -> IO ()    
nextMove rows bender = do
        let dir = currentDir bender
            directions = dir : (delete dir [SOUTH .. WEST])
            moves = map (tryMove rows bender) directions
            Just mv = find isJust moves
        fromJust mv

tryMove :: [String] -> Bender -> DIRECTION -> Maybe (IO ())
tryMove rows bender dir = let (newPosX, newPosY) = move (pos bender) dir
                              symbol = (rows !! newPosY) !! newPosX
                              withoutObstacle = removeX rows (newPosX, newPosY)
                              isBreaker = breaker bender
                          in case symbol of 
                              '@' -> Just $ putStrLn "LOOP"
                              '$' -> Just $ do putStrLn (show dir); return ()
                              '#' -> Nothing
                              'X' -> if isBreaker
                                     then Just $ printAndGo withoutObstacle dir $ Bender dir (newPosX, newPosY) isBreaker
                                     else Nothing
                              'B' -> Just $ printAndGo rows dir $ Bender dir (newPosX, newPosY) $ not isBreaker
                              'S' -> Just $ printAndGo rows dir $ Bender SOUTH (newPosX, newPosY) isBreaker
                              'W' -> Just $ printAndGo rows dir $ Bender WEST (newPosX, newPosY) isBreaker
                              'E' -> Just $ printAndGo rows dir $ Bender EAST (newPosX, newPosY) isBreaker
                              'N' -> Just $ printAndGo rows dir $ Bender NORTH (newPosX, newPosY) isBreaker
                              _ -> Just $ printAndGo rows dir $ Bender dir (newPosX, newPosY) isBreaker

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