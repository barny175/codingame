import System.IO
import Control.Monad
import Data.Maybe
import Data.List
import Data.Function

edges = [(6,2),(7,3),(6,3),(5,3),(3,4),(7,1),(2,0),(0,1),(0,3),(1,3),(2,3),(7,4),(6,5)]

gateways = [4,5]

si = 0

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    input_line <- getLine
    let input = words input_line
    let n = read (input!!0) :: Int -- the total number of nodes in the level, including the gateways
    let l = read (input!!1) :: Int -- the number of links
    let e = read (input!!2) :: Int -- the number of exit gateways
    --
    -- edges <- replicateM l $ do
    --     input_line <- getLine
    --     let input = words input_line
    --     let n1 = read (input!!0) :: Int -- N1 and N2 defines a link between these nodes
    --     let n2 = read (input!!1) :: Int
    --     return (n1, n2)
    --
    -- gateways <- replicateM e $ do
    --     input_line <- getLine
    --     let ei = read input_line :: Int -- the index of a gateway node
    --     return (ei)

    hPutStrLn stderr $ "Gateways:" ++ (show gateways)
    loop edges gateways

loop :: [(Int, Int)] -> [Int] -> IO ()
loop edges gateways = do
    input_line <- getLine
    -- let si = read input_line :: Int -- The index of the node on which the Skynet agent is positioned this turn

    hPutStrLn stderr $ "edges: " ++ (show edges) ++ " len: " ++ (show $ length edges)

    -- hPutStrLn stderr $ "paths: " ++ (show paths)
    -- hPutStrLn stderr $ "unused: " ++ (show unused) ++ " len: " ++ (show $ length unused)
    --
    let pathsToGw = findPathsToGw si edges gateways

    hPutStrLn stderr $ show pathsToGw

    let shortest = minimumBy (compare `on` length) pathsToGw


    hPutStrLn stderr $ show p

    let remainingEdges = delete p edges
    --
    -- hPutStrLn stderr $ "remaining edges: " ++ (show $ nub$ concat $ map (pathsFromEdge edges) paths)
    --
    putStrLn $ (show $ fst p) ++ " " ++ (show $ snd p)

    loop remainingEdges gateways

type Edge = (Int, Int)
type Path =[Edge]

edgeEndsInGw :: [Int] -> Edge -> Bool
edgeEndsInGw gws e = any (\g -> (g == (snd e)) || (g == (fst e))) gws

edgesFromNode :: [Edge] -> Int -> ([Edge], [Edge])
edgesFromNode edges node = partition startsIn edges
                where startsIn (a,b) = (node == b) || (node == a)

attachEdge :: Edge -> [Path] -> [Path]
attachEdge e [] = []
attachEdge e (p:ps) = (e:p) : (attachEdge e ps)

findPathsToGw :: Int -> [Edge] -> [Int] -> [Path]
findPathsToGw node edges gateways =
    let (edgesFromNode', remainingEdges) = edgesFromNode edges node
        directPathsToGw = map pure $ filter (edgeEndsInGw gateways) edgesFromNode'
        secondNode e = if fst e == node then snd e else fst e
        pathsFromEdge e = findPathsToGw (secondNode e) remainingEdges gateways
        indirectPaths = concat $ map (\e -> attachEdge e (pathsFromEdge e)) edgesFromNode'
    in directPathsToGw ++ indirectPaths
