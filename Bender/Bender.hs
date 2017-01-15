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
        bender = Bender SOUTH (posX, posY)
    direction rows bender
    return ()
    
data Bender = Bender { currentDir :: DIRECTION, pos :: (Int, Int) }

direction :: [String] -> Bender -> IO ()
direction rows bender = do 
    -- hPutStrLn stderr $ show pos
    
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
                          in case symbol of 
                              '@' -> Just $ putStrLn "LOOP"
                              '$' -> Just $ do putStrLn (show dir); return ()
                              '#' -> Nothing
                              'X' -> Nothing
                              _ -> Just $ printAndGo rows $ Bender dir (newPosX, newPosY)

printAndGo :: [String] -> Bender -> IO ()    
printAndGo rows bender = do
    putStrLn $ show $ currentDir bender
    direction rows bender
    return ()
    
move (posX, posY) SOUTH = (posX, posY + 1)
move (posX, posY) EAST = (posX + 1, posY)
move (posX, posY) NORTH = (posX, posY - 1)
move (posX, posY) WEST = (posX - 1, posY)
    

data DIRECTION =  SOUTH | EAST | NORTH | WEST 
                    deriving (Show, Enum, Eq)