import System.IO
import Control.Monad
import Data.List
import Data.Maybe

data Game = Game { dists :: [(Int, Int, Int)] , bombs :: Int } deriving Show

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
    
    -- hPutStrLn stderr $ show dists
    
    loop $ Game dists 2

loop game = do
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
        return (entityid, entitytype, player, arg2, arg3, arg4)
    
    let filtType tp = filter (\(_, t, _, _, _, _) -> t == tp) entities
        facts = map (\(eid, _, player, bs, prod, _) -> Factory eid bs prod player) $ filtType "FACTORY"
        filtFact n = filter ((== n) .player ) facts
        myFacts = filtFact 1
        neutralFacts = filtFact 0
        opFacts = filtFact (-1)
    
    -- troops
    let troops = map (\(tid, _, tplayer, _, ttarget, tborgs) -> Troop tid tplayer ttarget tborgs) $ filtType "TROOP"
    
    -- throw bombs
    let bombsOnWay = map (\(bid, _, plid, _, targetFact, _) -> Bomb bid plid targetFact) $ filtType "BOMB"
        bombing = bombCommand opFacts myFacts game bombsOnWay
    
    -- let possibleTargets = removeFactories opFacts $ map targetFact bombsOnWay
    -- let possibleTargets = recountFactories opFacts troops
    let possibleTargets = opFacts
    
    let condition = (\x y -> borgs x /= 0 && prod x > 0)
    
    debugA possibleTargets
    let closestO = closest myFacts possibleTargets (dists game) condition
    
    let possibleNeutralTargets = neutralFacts
        closestN = closest myFacts possibleNeutralTargets (dists game) condition
    
    let closestA = closestN ++ closestO
    
    debugA closestA
    -- increase production
    let incProdCommands =incProduction myFacts game
        
    debugA incProdCommands
    let commands = "WAIT": incProdCommands ++ map move closestA ++ bombing
    putStrLn $ concat $ intersperse ";" commands
    
    loop game { bombs = bombs game - length bombing }

debugA as = hPutStrLn stderr $ unlines $ map show as

incProduction facts game
    = let nonProducingsFactswithoutBorgs = closest facts facts (dists game) (\x y -> prod y == 0 && borgs y < 10 && borgs x > 0)
          moveCommands = map move nonProducingsFactswithoutBorgs
          zeroProd = filter (\f -> prod f == 0 && borgs f >= 10) facts
          incCommands = map (\f -> "INC " ++ (show $ eid f)) zeroProd
      in incCommands ++ moveCommands

recountFactories factories troops 
    = let troopToFact f = filter (\t -> ttarget t == eid f) troops
          troopAdd t = if tplayerid t == -1 then tborgs t else  (-tborgs t)
          sumOfTroops fact = foldl (\acc t-> acc + troopAdd t) (borgs fact) (troopToFact fact)
      in map (\f -> f { borgs = (sumOfTroops f)})  factories


removeFactory factories fact = let fid = eid fact
                               in filter (\f -> eid f /= fid) factories
               
removeFactories :: [Factory] -> [Int] -> [Factory]                
removeFactories factories toRemove = filter (\f -> not $ (eid f) `elem` toRemove) factories

-- bombCommand :: [Factory] -> [Factory] -> Game -> [String]
bombCommand opFacts myFacts game bombsOnWay =
    let isTarget f = isJust $ find (\b -> eid f == targetFact b && plid b == 1) bombsOnWay
        possibleTargets = filter (\f -> not $ isTarget f) opFacts
        toBomb = maximumBy (\x y -> compare (prod x) (prod y)) possibleTargets
        closestFact = head $ closest myFacts [toBomb] (dists game) (\_ _ -> True)
        sourceFact = fst closestFact
        bomb opFact = "BOMB " ++ (show $ eid $ sourceFact) ++ " " ++ (show $ eid opFact) 
    in if bombs game == 0 || null possibleTargets then [] else [bomb toBomb]
    
move (f1, f2) = "MOVE " ++ (show $ eid f1) ++ " " ++ (show $ eid f2) ++ " " ++ (show bs)
             where bs = 1 + (max 0 (borgs f2))
    

data Factory = Factory { eid :: Int, borgs :: Int, prod :: Int, player :: Int} deriving (Eq, Show)

data Troop = Troop {tid:: Int, tplayerid :: Int, ttarget :: Int, tborgs :: Int}

data Bomb = Bomb { bid :: Int, plid :: Int, targetFact :: Int } deriving Show

dist f1 f2 dists 
    = let id1 = eid f1
          id2 = eid f2
          dist' = find (\(e1, e2, d) -> (e1 == id1 && e2 == id2) || (e1 == id2 && e2 == id1)) dists
      in (\(_,_,d) -> d) <$> dist'
               
-- closest :: [Factory] -> [Factory] -> [(Int, Int, Int)] -> (Factory -> Factory -> Bool) -> [(Factory, Factory)]    
closest facts1 facts2 dists cond 
    = let ds = [(f1, f2) | f1 <- facts1, f2 <- facts2, 
                let d = dist f1 f2 dists, 
                isJust d,
                cond f1 f2]
          sorting (x,y) = dist x y dists
          sorted = sortOn sorting ds
      in sorted