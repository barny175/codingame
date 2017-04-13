import System.IO
import Control.Monad
import Data.Maybe
import Data.List
import Data.Function
import Control.Monad.Writer

-- edges = [(5,1),(1,2),(2,3),(3,4),(4,5),(0,1),(0,2),(0,3),(0,4),(0,5),(5,7),(1,8),(2,9),(3,10),(10,6),(7,8),(8,9),(3,9),(2,8),(4,10),(7,12),(4,14),(14,6),(13,5),(13,6),(1,15),(6,16),(7,17),(17,6),(10,18),(9,19),(6,11),(12,1),(7,20),(21,9),(21,10),(21,3)]
-- gateways = [11,12,15,16,18,19,20]

-- edges = [(0, 1), (1,2), (0, 3), (3,2)]
-- gateways = [2]

-- edges = [(0, 1), (1,2), (0, 3), (3,2), (0, 2)]
-- gateways = [2]

-- edges = [(0, 1), (0,2), (2, 4), (1, 3), (3,4), (1, 4), (1,2), (2,5), (0,3)]
-- gateways = [4, 5]

-- gateways = [0,16,18,26]
-- edges = [(2,5),(14,13),(16,13),(19,21),(13,7),(16,8),(35,5),(2,35),(10,0),(8,3),(23,16),(0,1),(31,17),(19,22),(12,11),(1,2),(1,4),(14,9),(17,16),(30,29),(32,22),(28,26),(24,23),(20,19),(15,13),(18,17),(6,1),(29,28),(15,14),(9,13),(32,18),(25,26),(1,7),(34,35),(33,34),(27,16),(27,26),(23,25),(33,3),(16,30),(25,24),(3,2),(5,4),(31,32),(27,25),(19,3),(17,8),(4,2),(32,17),(10,11),(29,27),(30,27),(6,4),(24,15),(9,10),(34,2),(9,7),(11,6),(33,2),(14,10),(12,6),(0,6),(19,17),(20,3),(21,20),(21,32),(15,16),(0,9),(23,27),(11,0),(28,27),(22,18),(3,1),(23,15),(18,19),(7,0),(19,8),(21,22),(7,36),(13,36),(8,36)]


edges = [(6,2),(7,3),(6,3),(5,3),(3,4),(7,1),(2,0),(0,1),(0,3),(1,3),(2,3),(7,4),(6,5)]
gateways = [4,5]

si = 2

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- input_line <- getLine
    -- let input = words input_line
    -- let n = read (input!!0) :: Int -- the total number of nodes in the level, including the gateways
    -- let l = read (input!!1) :: Int -- the number of links
    -- let e = read (input!!2) :: Int -- the number of exit gateways
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

    -- hPutStrLn stderr $ "Gateways:" ++ (show gateways)
    loop edges gateways
--
loop :: [Edge] -> [Int] -> IO ()
loop edges gateways = do
    input_line <- getLine
    let si = read input_line :: Int -- The index of the node on which the Skynet agent is positioned this turn

    -- hPutStrLn stderr $ "edges: " ++ (show edges) ++ " len: " ++ (show $ length edges)

    let pathsToGw = findPathsToGw si edges gateways

    -- hPutStrLn stderr $ show $ pathsToGw

    let lengthComparator = compare `on` length
        gwsForPath p = countEdgesToGateway (getGatewayFromPath p gateways) edges
        gwCountComparator = compare `on` gwsForPath

    let lastNode p = if fst le `elem` gateways then snd le else fst le where le = last p
        edgesToGateways = nub $ concat $ map (fst . edgesFromNode edges) gateways
        hotspots' = hotspots gateways edgesToGateways
        hotEdges = map edgeToGw hotspots' where edgeToGw node = last $ filter (edgeEndsInGw gateways) (fst $ edgesFromNode edges node)

    let comparator x y = (gwCountComparator x y) `mappend` (lengthComparator x y)
        shortest = minimumBy comparator pathsToGw

    let directPaths = filter ((==) 1 . length) pathsToGw
        p = if not $ null directPaths
            then last (head directPaths)
            else if not $ null hotEdges then
                    head hotEdges
                 else
                    last shortest

    -- hPutStrLn stderr $ "Edges to gateways: " ++ (show edgesToGateways)
    -- hPutStrLn stderr $ show p
    putStrLn $ (show $ fst p) ++ " " ++ (show $ snd p)

    let remainingEdges = delete p edges
    loop remainingEdges gateways

hotspots gateways edgesToGateways
    = let hotnodes = map head . sortBy (compare `on` length) . filter ((>1) . length) . group . sort . concat . map (\(e, f) -> [e,f])
      in filter (flip notElem $ gateways) (hotnodes edgesToGateways)

countEdgesToGateway g edges = length $ filter (flip edgeEndsInNode g) edges

getGatewayFromPath p gateways = if last1 `elem` gateways then last1 else last2
                             where (last1, last2) = last p

edgeEndsInNode e n = fst e == n || snd e == n

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

directPathsToGw gateways edgesFromNode' = map pure $ filter (edgeEndsInGw gateways) edgesFromNode'

findPathsToGw :: Int -> [Edge] -> [Int] -> [Path]
findPathsToGw node edges gateways
    | node `elem` gateways = []
    | otherwise =
        let (edgesFromNode', remainingEdges) = edgesFromNode edges node
            secondNode e = if fst e == node then snd e else fst e
            directPathsToGw = map pure $ filter (edgeEndsInGw gateways) edgesFromNode'
            neighbours = nub $ concat $ map (\(e,f) -> [e,f]) edgesFromNode'
            filteredEdges = filter (\(e,f) -> not $ e `elem` neighbours && f `elem` neighbours) remainingEdges
            pathsFromEdge e = findPathsToGw (secondNode e) filteredEdges gateways
            indirectPaths = concat $ map (\e -> attachEdge e (pathsFromEdge e)) edgesFromNode'
        in directPathsToGw ++ indirectPaths
