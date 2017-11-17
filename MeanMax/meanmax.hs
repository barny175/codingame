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
        let unittype = read (input!!1) :: Int
        let player = read (input!!2) :: Int
        let _mass = read (input!!3) :: Float
        let _radius = read (input!!4) :: Int
        let x = read (input!!5) :: Int
        let y = read (input!!6) :: Int
        let vx = read (input!!7) :: Int
        let vy = read (input!!8) :: Int
        let extra = read (input!!9) :: Int
        let extra2 = read (input!!10) :: Int
        if unittype == 4 then
            return $ Wreck { posX = x, posY = y, wreckid = _unitid, radius = _radius }
        else
            return $ Reaper { reaperid =_unitid, mass = _mass , rX = x, rY = y }

    -- hPutStrLn stderr $ unlines $ map show units

    let isReaper (Reaper _ _ _ _) = True
        isReaper _ = False
        Just reaper = find (\r -> reaperid r == 0) $ filter isReaper units
        wrecks = sortOn (dist reaper) $ filter (not . isReaper) units
        wreck = head $ wrecks

    -- hPutStrLn stderr $ unlines $ map show wrecks
    -- hPutStrLn stderr $ show reaper
    hPutStrLn stderr $ unlines $ map (\w -> show w ++ " " ++ (show $ dist reaper w)) wrecks

    putStrLn $ if null wrecks then "WAIT" else (\w -> throttle reaper (posX w) (posY w)) wreck
    putStrLn "WAIT"
    putStrLn "WAIT"

    loop

r = 400

dist :: Unit -> Unit -> Int
dist (Reaper _ x1 y1 _) (Wreck x2 y2 _ _) = distance x1 y1 x2 y2

distance :: Int -> Int -> Int -> Int -> Int
distance x1 y1 x2 y2 = floor $ sqrt $ fromIntegral ((x1 - x2)^2 + (y1 - y2)^2)

throttle :: Unit -> Int -> Int -> String
throttle r x1 y1 =
    let x2 = rX r
        y2 = rY r
        dirx = x1 -- x2 + r `div` (1 + y1 - y2) * (x1 - x2)
        diry = y1  -- y2 + r `div` (1 + y1 - y2) * (y1 - y2)
        power = max ((distance x1 y2 x2 y2) `div` 10) 100
    in (show dirx) ++ " " ++ (show $ diry) ++ " " ++ (show power)

data Unit = Reaper { mass :: Float, rX :: Int, rY :: Int, reaperid :: Int }
            | Wreck {posX :: Int, posY :: Int, wreckid :: Int, radius :: Int}
            deriving Show
