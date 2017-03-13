import System.IO
import Control.Monad
import Data.Maybe
import Data.List
import Data.Function

-- edges = [(5,1),(1,2),(2,3),(3,4),(4,5),(0,1),(0,2),(0,3),(0,4),(0,5),(5,7),(1,8),(2,9),(3,10),(10,6),(7,8),(8,9),(3,9),(2,8),(4,10),(7,12),(4,14),(14,6),(13,5),(13,6),(1,15),(6,16),(7,17),(17,6),(10,18),(9,19),(6,11),(12,1),(7,20),(21,9),(21,10),(21,3)]
-- gateways = [11,12,15,16,18,19,20]

-- edges = [(0, 1), (1,2), (0, 3), (3,2)]
-- gateways = [2]

-- edges = [(0, 1), (1,2), (0, 3), (3,2), (0, 2)]
-- gateways = [2]

edges = [(0, 1), (0,2), (2, 4), (1, 3), (3,4), (1, 4), (1,2), (2,5), (0,3)]
gateways = [4, 5]

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

    let lengthComparator = compare `on` length
        gwsForPath p = countEdgesToGateway (getGatewayFromPath p gateways) edges
        gwCountComparator = compare `on` gwsForPath
        shortest = minimumBy (lengthComparator `mappend` gwCountComparator) pathsToGw
        p = last shortest

    hPutStrLn stderr $ show p

    let remainingEdges = delete p edges
    --
    -- hPutStrLn stderr $ "remaining edges: " ++ (show $ nub$ concat $ map (pathsFromEdge edges) paths)
    --
    putStrLn $ (show $ fst p) ++ " " ++ (show $ snd p)

    loop remainingEdges gateways

countEdgesToGateway g edges = length $ filter (flip edgeEndsInNode g) edges

getGatewayFromPath p gateways = if last1 `elem` gateways then last1 else last2
                                where (last1, last2) = last p

type Edge = (Int, Int)
type Path =[Edge]

edgeEndsInNode e n = fst e == n || snd e == n

edgeEndsInGw :: [Int] -> Edge -> Bool
edgeEndsInGw gws e = any (edgeEndsInNode e) gws

edgesFromNode :: [Edge] -> Int -> ([Edge], [Edge])
edgesFromNode edges node = partition startsIn edges
                where startsIn (a,b) = (node == b) || (node == a)

attachEdge :: Edge -> [Path] -> [Path]
attachEdge e [] = []
attachEdge e (p:ps) = (e:p) : (attachEdge e ps)

findPathsToGw :: Int -> [Edge] -> [Int] -> [Path]
findPathsToGw node edges gateways
  | node `elem` gateways = []
  | otherwise =
    let (edgesFromNode', remainingEdges) = edgesFromNode edges node
        directPathsToGw = map pure $ filter (edgeEndsInGw gateways) edgesFromNode'
        secondNode e = if fst e == node then snd e else fst e
        pathsFromEdge e = findPathsToGw (secondNode e) remainingEdges gateways
        indirectPaths = concat $ map (\e -> attachEdge e (pathsFromEdge e)) edgesFromNode'
    in directPathsToGw ++ indirectPaths
