import System.IO
import Control.Monad

import Data.List

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    loop

loop :: IO ()
loop = do
    input_line <- getLine
    let myscore = read input_line :: Int
    input_line <- getLine
    let enemyscore1 = read input_line :: Int
    input_line <- getLine
    let enemyscore2 = read input_line :: Int
    input_line <- getLine
    let myrage = read input_line :: Int
    input_line <- getLine
    let enemyrage1 = read input_line :: Int
    input_line <- getLine
    let enemyrage2 = read input_line :: Int
    input_line <- getLine
    let unitcount = read input_line :: Int

    units <- replicateM unitcount $ do
        input_line <- getLine
        let input = words input_line
        let _unitid = read (input!!0) :: Int
        let _unittype = read (input!!1) :: Int
        let player = read (input!!2) :: Int
        let _mass = read (input!!3) :: Float
        let _radius = read (input!!4) :: Int
        let x = read (input!!5) :: Int
        let y = read (input!!6) :: Int
        let _vx = read (input!!7) :: Int
        let _vy = read (input!!8) :: Int
        let extra = read (input!!9) :: Int
        let extra2 = read (input!!10) :: Int
        return $ Unit { posX = x, posY = y, unitid = _unitid, radius = _radius, mass = _mass , vX = _vx, vY = _vy, unittype = _unittype, playerid = player }

    hPutStrLn stderr $ unlines $ map show units

    let Just reaper = find (\r -> playerid r == 0) $ unitsOfType units 0
        Just destroyer = find (\r -> playerid r == 0) $ unitsOfType units 1
        wrecks = sortOn (dist reaper) $ unitsOfType units 4
        tankers = sortOn (dist destroyer) $ unitsOfType units 3
        wreck = head $ wrecks

    -- hPutStrLn stderr $ unlines $ map show wrecks
    -- hPutStrLn stderr $ show reaper
    -- hPutStrLn stderr $ unlines $ map (\w -> show w ++ " " ++ (show $ dist reaper w)) wrecks

    putStrLn $ if null wrecks then throttle reaper destroyer else throttle reaper wreck
    putStrLn $ if null tankers then "WAIT" else throttle destroyer (head tankers)
    putStrLn "WAIT"

    loop

unitsOfType units utype =  filter (\u -> unittype u == utype) units

r = 400

dist :: Unit -> Unit -> Int
dist u1 u2 = let x1 = posX u1
                 x2 = posX u2
                 y1 = posY u1
                 y2 = posY u2
             in distance x1 y1 x2 y2

distance :: Int -> Int -> Int -> Int -> Int
distance x1 y1 x2 y2 = floor $ sqrt $ fromIntegral ((x1 - x2)^2 + (y1 - y2)^2)

throttle :: Unit -> Unit -> String
throttle r w =
    let x2 = posX r
        y2 = posY r
        x1 = posX w
        y1 = posY w
        dirx = x1
        diry = y1
        d = (distance x1 y2 x2 y2)
        inWreck = d < radius w
        power = if d < 100 then 100 else 200
    in (show dirx) ++ " " ++ (show $ diry) ++ " " ++ (show power)

data Unit = Unit { posX :: Int, posY :: Int, vX :: Int, vY :: Int, mass :: Float , unitid :: Int, radius :: Int, unittype :: Int, playerid :: Int}
                deriving Show

-- 0 (Reaper), 1 (Destroyer), 3 (Tanker), 4 (Wreck)
