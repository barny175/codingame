import System.IO
import Control.Monad
import Data.List

loop :: Int -> Int -> IO ()
loop teamId magic = do
    input_line <- getLine
    let entities = read input_line :: Int -- number of entities still in game
    
    entities <- replicateM entities $ do
        input_line <- getLine
        let input = words input_line
        let entityid = read (input!!0) :: Int -- entity identifier
        let entitytype = input!!1 -- "WIZARD", "OPPONENT_WIZARD" or "SNAFFLE" (or "BLUDGER" after first league)
        let x = read (input!!2) :: Int -- position
        let y = read (input!!3) :: Int -- position
        let vx = read (input!!4) :: Int -- velocity
        let vy = read (input!!5) :: Int -- velocity
        let state = read (input!!6) :: Int -- 1 if the wizard is holding a Snaffle, 0 otherwise
        return (entityid, entitytype, x, y ,vx, vy, state)
    
    hPutStrLn stderr $ show magic
    let snaffles = filterEntities entities "SNAFFLE"
    let bludgers = filterEntities entities "BLUDGER"
    let wizards = filterEntities entities "WIZARD" 
    
    hPutStrLn stderr $ show snaffles
    hPutStrLn stderr $ show bludgers
    hPutStrLn stderr $ show wizards
    
    let w1 = wizards !! 0
        w2 = wizards !! 1
        
    let comp a b = compare (entId $ fst a) (entId $ fst b)   
        closest = sortBy comp $ closestSnaffles [w1, w2] snaffles
    
    hPutStrLn stderr $ show closest
    
    forM closest $ action magic teamId bludgers
            
    loop teamId (magic + 1)
            
action :: Int -> Int -> [Entity] -> (Entity, Entity) -> IO ()
action magic teamId bludgers (wizard, snaffle) = do
    let (closestBludger, dist) = findClosest bludgers wizard
    
    if magic > 5 && dist < 800 then
        obliviate $ entId closestBludger
    else  
        if state wizard == 1 then do 
            throw wizard teamId
        else
            move (wizard, snaffle)

type Point = (Int, Int)

distance :: Entity -> Entity -> Float
distance ent1 wizard = let x1 = getX ent1
                           x2 = getX wizard
                           y1 = getY ent1
                           y2 = getY wizard
                       in sqrt $ fromIntegral ((x1-x2)^2 + (y1-y2)^2) 
                       
filterEntities :: [(Int, String, Int, Int, Int, Int, Int)] -> String -> [(Int, String, Int, Int, Int, Int, Int)]  
filterEntities entities entityType = filter predicate entities
                                  where predicate (_, entType, _, _, _, _, _) = entType == entityType

throw :: Entity -> Int -> IO ()
throw wizard teamId = do
    let (goalX, goalY) = if teamId == 0 
               then (16000, 3750) 
               else (0, 3750)
    putStrLn ("THROW " ++ (show goalX) ++ " " ++ (show goalY) ++ " 500")
    
findClosest :: [Entity] -> Entity -> (Entity, Float)
findClosest entities wizard = let comparator a b = compare (snd a) (snd b) 
                                  dists = map (\s -> (s, distance s wizard)) entities
                              in minimumBy comparator dists

closestSnaffles :: [Entity] -> [Entity] -> [(Entity, Entity)]
closestSnaffles (w:[]) (s:[]) = [(w, s)]
closestSnaffles [] _ = []
closestSnaffles wizards snaffles = let distances = [(w, s, distance w s)| w <- wizards, s <- snaffles]
                                       comparator (w1, s1, d1) (w2, s2, d2) = compare d1 d2
                                       (closestW, closestS, _) = minimumBy comparator distances
                                       remainingSnaffles = delete closestS snaffles
                                   in (closestW, closestS) : closestSnaffles (delete closestW wizards) (if null remainingSnaffles then [closestS] else remainingSnaffles)

move :: (Entity, Entity) -> IO ()
move (w,s) = putStrLn ("MOVE " ++ (show sx) ++ " " ++ (show sy) ++ " 150")
             where sx = getX s
                   sy = getY s 
 
removeThrownSnaffle :: [Entity] -> Entity -> [Entity]
removeThrownSnaffle snaffles wizard = let Just thrownSnaffle = find (\sn -> (getX sn == getX wizard) && (getY sn == getY wizard) ) snaffles
                                      in delete thrownSnaffle snaffles
          
obliviate :: Int -> IO ()
obliviate bid = putStrLn $ "OBLIVIATE " ++ (show bid)

type Entity = (Int, String, Int, Int, Int, Int, Int)
    
state :: Entity -> Int
state (_, _, _, _, _, _, st) = st

getX :: Entity -> Int
getX (_, _, x, _, _, _, _) = x

getY :: Entity -> Int
getY (_, _, _, y, _, _, _) = y
 

pos :: Entity -> Point
pos ent = (getX ent, getY ent)

speed :: Entity -> Point
speed (_, _, _, _, vx, vy, _) = (vx, vy)

entId :: Entity -> Int
entId (eid, _, _, _, _, _, _) = eid

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Grab Snaffles and try to throw them through the opponent's goal!
    -- Move towards a Snaffle and use your team id to determine where you need to throw it.
    
    input_line <- getLine
    let myteamid = read input_line :: Int -- if 0 you need to score on the right of the map, if 1 you need to score on the left
    loop myteamid 0

