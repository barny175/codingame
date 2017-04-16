import System.IO
import Control.Monad
import Data.List
import Data.Function
import Data.Maybe

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    loop

data Entity = Ship {sid :: Int, x :: Int, y :: Int, orientation :: Int, speed :: Int, rum :: Int, owner :: Int, etype :: String, canFire :: Bool }
              | Barrel {bid :: Int, x :: Int, y :: Int, amount :: Int, etype :: String  }
              | Mine { mid :: Int, x :: Int, y :: Int, etype :: String  }
              | Cannonball { cbid :: Int, x :: Int, y :: Int, tti :: Int , owner :: Int, etype :: String  }
              deriving (Show, Eq)

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
        case entitytype of
            "SHIP" -> return $ Ship { sid = entityid, x = x, y = y, orientation = arg1, speed = arg2, rum = arg3, owner = owner, etype = entitytype, canFire = True }
            "BARREL" -> return $ Barrel { bid = entityid, x = x, y = y, amount = arg1, etype = entitytype }
            "CANNONBALL"-> return $ Cannonball { cbid = entityid, x = x, y = y, owner = arg1, tti = arg2, etype = entitytype }
            "MINE" -> return $ Mine { mid = entityid, x = x, y = y, etype = entitytype  }

loop :: IO ()
loop = do
    input_line <- getLine
    let myshipcount = read input_line :: Int -- the number of remaining ships
    input_line <- getLine
    let entitycount = read input_line :: Int -- the number of entities (e.g. ships, mines or cannonballs)

    entities <- readEntities entitycount

    let ships = filter ((== "SHIP").etype) entities
        barrels =  filter ((== "BARREL").etype) entities
        (myShips, hisShips) = partition ((==1) . owner) ships
        closest' = closest myShips barrels

    let myShip = head myShips
        hisShip = head hisShips
        shipDist = cubeDistance (x myShip, y myShip) (x hisShip, y hisShip)
        firePos = fire myShip hisShip

    if shipDist < 10 && canFire myShip && isJust firePos then do
        let (Just fp) = firePos
        putStrLn ("FIRE " ++ (show $ fst fp) ++ " " ++ (show $ snd fp))
    else
        if null closest' then
            putStrLn "WAIT"
        else
            forM_ closest' $ \(sh, b) -> do
                hPutStrLn stderr $ show (bid b)
                putStrLn ("MOVE "++ (show $ x b) ++ " " ++ (show $ y b))

    loop

directions_even = [[1, 0], [0, (-1)], [(-1), (-1)], [(-1), 0], [(-1), 1], [0, 1]]
directions_odd = [[1, 0], [1, (-1)], [0, -1], [(-1), 0], [0, 1], [1, 1]]

neighbour x y orientation = if y `mod` 2 == 1 then
                              (x + (directions_odd !! orientation) !! 0, y + (directions_odd !! orientation) !! 1)
                           else
                              (x + directions_even !! orientation !! 0, y + directions_even !! orientation !! 1)

futurePos ship 1 = neighbour (x ship) (y ship) (orientation ship)
futurePos ship t =
    let (nx, ny) = if speed ship == 1 then neighbour (x ship) (y ship) (orientation ship) else (x ship, y ship)
    in futurePos ship {x = nx, y = ny } (t - 1)

bow s = neighbour (x s) (y s) (orientation s)

validPos (x, y)
    | x >= 0 && x <= 23 && y >= 0 && y <= 21 = True
    | otherwise = False

-- fire :: Ship -> Ship -> (Int, Int)
fire from to =
    let futurePoses = filter (validPos.snd) $ map (\t -> (t, futurePos to t)) [1..10]
        travelTime (px, py) = 1 + (cubeDistance (x from, y from) (px, py)) `div` 3
        travelTimes = map (\(t, p) -> (t - travelTime p, p)) futurePoses
        cbpos = sortBy (compare `on` (abs.fst) ) travelTimes
    in snd <$> listToMaybe cbpos

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
closest [] _ = []
closest _ [] = []
closest (w:[]) (s:[]) = [(w, s)]
closest wizards snaffles =
    let distances = [(w, s, distance w s)| w <- wizards, s <- snaffles]
        comparator (w1, s1, d1) (w2, s2, d2) = compare d1 d2
        (closestW, closestS, _) = minimumBy comparator distances
        remaining = delete closestS snaffles
    in (closestW, closestS) : closest (delete closestW wizards) (if null remaining then [closestS] else remaining)

toCubeCoord q r
    = let xp = q
          zp = r
          yp = -(xp + zp)
      in (xp, yp, zp)

cubeDistance (q1, r1) (q2, r2) =
    let (x1, y1, z1) = toCubeCoord q1 r1
        (x2, y2, z2) = toCubeCoord q2 r2
    in (abs(x1 - x2) + abs(y1 - y2) + abs(z1 - z2)) `div` 2
