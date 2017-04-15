import System.IO
import Control.Monad
import Data.List

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    loop

readEntities entitycount = 
    replicateM entitycount $ do
        input_line <- getLine
        let input = words input_line
        let entityid = read (input!!0) :: Int
        let entitytype = input!!1
        let x = read (input!!2) :: Int
        let y = read (input!!3) :: Int
        let arg1 = read (input!!4) :: Int
        let arg2 = read (input!!5) :: Int
        let arg3 = read (input!!6) :: Int
        let owner = read (input!!7) :: Int
        if entitytype == "SHIP" then
            return $ Ship { sid = entityid, x = x, y = y, rotation = arg1, speed = arg2, rum = arg3, owner = owner }
        else
            return $ Barrel { bid = entityid, x = x, y = y, amount = arg1}
       
       
loop :: IO ()
loop = do
    input_line <- getLine
    let myshipcount = read input_line :: Int -- the number of remaining ships
    input_line <- getLine
    let entitycount = read input_line :: Int -- the number of entities (e.g. ships, mines or cannonballs)
    
    entities <- readEntities entitycount
    
    let (ships, barrels) = partition (\x -> case x of Barrel _ _ _ _ -> False; _ -> True ) entities 
        myShips = filter ((==1) . owner) ships
        closest' = closest myShips barrels
    hPutStrLn stderr $ unlines (map show closest')

    forM_ closest' $ \(sh, b) -> do
        hPutStrLn stderr $ show (bid b)
        putStrLn ("MOVE "++ (show $ x b) ++ " " ++ (show $ y b))
        return ()
    
    loop
    
data Entity = Ship {sid :: Int, x :: Int, y :: Int, rotation :: Int, speed :: Int, rum :: Int, owner :: Int }
              | Barrel {bid :: Int, x :: Int, y :: Int, amount :: Int }
              deriving (Show, Eq)

distance :: Entity -> Entity -> Float
distance ent1 ent2 = 
    let x1 = x ent1
        x2 = x ent2
        y1 = y ent1
        y2 = y ent2
    in sqrt $ fromIntegral ((x1-x2)^2 + (y1-y2)^2) 

findClosest :: [Entity] -> Entity -> (Entity, Float)
findClosest entities ent = 
    let comparator a b = compare (snd a) (snd b) 
        dists = map (\s -> (s, distance s ent)) entities
    in minimumBy comparator dists

closest :: [Entity] -> [Entity] -> [(Entity, Entity)]
closest (w:[]) (s:[]) = [(w, s)]
closest [] _ = []
closest wizards snaffles = 
    let distances = [(w, s, distance w s)| w <- wizards, s <- snaffles]
        comparator (w1, s1, d1) (w2, s2, d2) = compare d1 d2
        (closestW, closestS, _) = minimumBy comparator distances
        remaining = delete closestS snaffles
    in (closestW, closestS) : closest (delete closestW wizards) (if null remaining then [closestS] else remaining)
