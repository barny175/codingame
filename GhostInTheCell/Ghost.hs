import System.IO
import Control.Monad
import Data.List
import Data.Maybe

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    input_line <- getLine
    let factorycount = read input_line :: Int -- the number of factories
    input_line <- getLine
    let linkcount = read input_line :: Int -- the number of links between factories
    
    dists <- replicateM linkcount $ do
        input_line <- getLine
        let input = words input_line
        let factory1 = read (input!!0) :: Int
        let factory2 = read (input!!1) :: Int
        let distance = read (input!!2) :: Int
        return (factory1, factory2, distance)
    
    hPutStrLn stderr $ show dists
    
    loop dists

loop dists = do
    input_line <- getLine
    let entitycount = read input_line :: Int -- the number of entities (e.g. factories and troops)
    
    entities <- replicateM entitycount $ do
        input_line <- getLine
        let input = words input_line
        let entityid = read (input!!0) :: Int
        let entitytype = input!!1
        let player = read (input!!2) :: Int
        let arg2 = read (input!!3) :: Int
        let arg3 = read (input!!4) :: Int
        let arg4 = read (input!!5) :: Int
        let arg5 = read (input!!6) :: Int
        return (entityid, entitytype, player, arg2, arg3)
    
    let filtType tp = filter (\(_, t, _, _, _) -> t == tp) entities
        facts = map (\(eid, _, player, bs, prod) -> Factory eid bs prod player) $ filtType "FACTORY"
        factsWithTroops = map 
        troops = filtType "TROOP"
        filtFact n = filter ((== n) .player ) facts
        myFacts = filtFact 1
        neutralFacts = filtFact 0
        closestN = closest myFacts neutralFacts dists (\x y -> borgs x /= 0 && borgs x > borgs y) 
        opFacts = filtFact (-1)
        closestO = closest myFacts opFacts dists (\x y -> borgs x /= 0 && borgs x > borgs y)
        closestA = closestN ++ closestO
    hPutStrLn stderr $ show facts
    hPutStrLn stderr $ show closestN
    hPutStrLn stderr $ show closestO
    
    putStrLn $ concat $ intersperse ";" ("WAIT": map move closestA)
    
    loop dists

move (f1, f2) = "MOVE " ++ (show $ eid f1) ++ " " ++ (show $ eid f2) ++ " " ++ (show 1)
             where bs = 1 + (max 0 (borgs f2))
    

data Factory = Factory { eid :: Int, borgs :: Int, prod :: Int, player :: Int} deriving (Eq, Show)

data Troop = Troop {tid:: Int, playerid :: Int, target :: Int, troopBorgs :: Int, arrives :: Int}

dist f1 f2 dists = let id1 = eid f1
                       id2 = eid f2
                       dist' = find (\(e1, e2, d) -> (e1 == id1 && e2 == id2) || (e1 == id2 && e2 == id1)) dists
                   in (\(_,_,d) -> d) <$> dist'


nubIt :: [(Int, Factory, Factory)] -> [(Int, Factory, Factory)]                               
nubIt [] = []
nubIt (x:xs) = let rest = filter ((/= (second x)).second) xs
                   second (_, y, _) = y
               in x : (nubIt rest)
               
closest :: [Factory] -> [Factory] -> [(Int, Int, Int)] -> (Factory -> Factory -> Bool) -> [(Factory, Factory)]    
closest facts1 facts2 dists cond = let ds = [(fromJust d, f1, f2) | f1 <- facts1, f2 <- facts2, 
                                             let d = dist f1 f2 dists, 
                                             isJust d,
                                             cond f1 f2]
                                       first (d, _, _) = d
                                       sorted = sortOn first ds
                                    in map (\(_, e, f) -> (e, f)) sorted