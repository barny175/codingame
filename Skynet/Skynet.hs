import System.IO
import Control.Monad
import qualified Data.Set as Set
import Data.List
import Data.Maybe

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    input_line <- getLine
    let input = words input_line
    let n = read (input!!0) :: Int -- the total number of nodes in the level, including the gateways
    let l = read (input!!1) :: Int -- the number of links
    let e = read (input!!2) :: Int -- the number of exit gateways

    edges <- replicateM l $ do
        input_line <- getLine
        let input = words input_line
        let n1 = read (input!!0) :: Int -- N1 and N2 defines a link between these nodes
        let n2 = read (input!!1) :: Int
        return (n1 ,n2)


    gateways <- replicateM e $ do
        input_line <- getLine
        let ei = read input_line :: Int -- the index of a gateway node
        return ei

    hPutStrLn stderr $ "Gateways: " ++ ( show gateways)

    loop edges gateways

loop :: [(Int, Int)] -> [Int] -> IO ()
loop edges gateways = do
    input_line <- getLine
    let si = read input_line :: Int -- The index of the node on which the Skynet agent is positioned this turn

    hPutStrLn stderr $ "edges: " ++ (show edges) ++ " len: " ++ (show $ length edges)

    let (paths, unused) = pathsFromPoint edges si

    hPutStrLn stderr $ "paths: " ++ (show paths)
    hPutStrLn stderr $ "unused: " ++ (show unused) ++ " len: " ++ (show $ length unused)

    let pathToGw = findEdgeToGw paths gateways

    --hPutStrLn stderr $ show edgeToGw

    let p = if isJust pathToGw
            then fromJust pathToGw
            else fromJust $ findPathToGw paths unused gateways

    --hPutStrLn stderr $ show p

    let remainingEdges = delete p edges

    hPutStrLn stderr $ "remaining edges: " ++ (show $ nub$ concat $ map (pathsFromEdge edges) paths)

    putStrLn $ (show $ fst p) ++ " " ++ (show $ snd p)

    loop remainingEdges gateways


findEdgeToGw :: [Edge] -> [Int] -> Maybe Edge
findEdgeToGw paths gws = find isGw paths
                     where isGw p = edgeEndsInGw p gws

type Edge = (Int, Int)

edgeEndsInGw :: Edge -> [Int] -> Bool
edgeEndsInGw e gws = any (\g -> (g == (snd e)) || (g == (fst e))) gws

pathsFromPoint :: [Edge] -> Int -> ([Edge], [Edge])
pathsFromPoint edges point = partition startsIn edges
                where startsIn (a,b) = (point == b) || (point == a)

pathsFromEdge :: [Edge] -> Edge -> [Edge]
pathsFromEdge edges e = filter startsIn edges
                where startsIn (a,b) = (fst e == b) || (fst e == a) || (snd e == a) || (snd e == b)

pathsFromEdges ::[Edge] -> [Edge] -> [Edge]
pathsFromEdges availableEdges edges = concat $ map (pathsFromEdge availableEdges) edges

findPathToGw :: [Edge] -> [Edge] -> [Int] -> Maybe (Int, Int)
--findPathToGw ps [] _ = error $ "p: " ++ (show ps)
--findPathToGw [] es _ = error $ "should not happen" ++ (show es) ++ " len: " ++ (show $ length es)
findPathToGw ps edges gateways =
    let nextEdges = pathsFromEdges edges ps
        edgeToGw = findEdgeToGw nextEdges gateways
        unusedEdges = edges \\ nextEdges
    in if isJust edgeToGw then edgeToGw
                          else findPathToGw nextEdges unusedEdges gateways
                            
