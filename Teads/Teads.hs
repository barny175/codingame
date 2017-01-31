import System.IO
import Control.Monad
import Data.List

main :: IO ()
main = do
    input_line <- getLine
    let n = read input_line :: Int -- the number of adjacency relations
    let readInt i = read i ::Int

    edges <- replicateM n $ do
        input <- map (readInt).words <$> getLine
        return (input!!0, input!!1)

    -- hPutStrLn stderr $ show $ length relations

    -- let node = 101
    let node = fst $ head edges

    let tree = buildTree edges node
    -- hPutStrLn stderr $ "Tree: \n" ++ (show tree)

    -- hPutStrLn stderr $ show $ getSecondMaxDepth (subtrees tree) 6
    hPutStrLn stderr "Working"
    let (result, debug) = findMinDepth tree 0
    -- hPutStrLn stderr $ debug
    putStrLn $ show $ result

getSecondMaxDepth :: [Tree] -> Int -> Int
getSecondMaxDepth sts maxDepth = foldl (secondBiggest maxDepth) 0 sts

secondBiggest :: Int -> Int -> Tree -> Int
secondBiggest biggest acc s = let curDepth = depth s
                              in if curDepth > acc && curDepth < biggest
                                 then curDepth
                                 else acc

findMinDepth :: Tree -> Int -> (Int, String)
findMinDepth tree depthOfSkippedSubtree
    | maxSubtreeCount > 1 = (dpth, "root=" ++ (show $ root tree) ++ " maxsubtreecount=" ++ (show maxSubtreeCount)++ " depth=" ++ (show dpth))
    | (dpth - 1) < skippedSubtreeDepth = (dpth, "tree=" ++ (show $ root tree) ++ " skipped=" ++ (show depthOfSkippedSubtree) ++ " depth=" ++ (show dpth))
    | otherwise = (recursiveDetph, "root=" ++ (show $ root tree) ++ " skipped=" ++ (show depthOfSkippedSubtree) ++ " subtree=" ++ (show $ root $ head maxSubtree) ++ " subtree depth=" ++ (show $ depth $ head maxSubtree)  ++ "-> " ++ recursiveStr)
    where
        dpth = depth tree
        maxSubtree = getSubtreesByDepth tree (dpth - 1)
        maxSubtreeCount = length maxSubtree
        subtrs = subtrees tree
        secondMaxDepth = (getSecondMaxDepth subtrs (dpth - 1))
        skippedSubtreeDepth = (max depthOfSkippedSubtree (secondMaxDepth + 1)) + 1
        (recursiveDetph, recursiveStr) = findMinDepth (head maxSubtree) skippedSubtreeDepth

getSubtreesByDepth :: Tree -> Int -> [Tree]
getSubtreesByDepth tree dpth = filter (\subtree -> depth subtree == dpth) (subtrees tree)

buildTree :: [Edge] -> Node -> Tree
buildTree [] node = Tree node 0 []
buildTree edges node = let (edgesFromNode, remainingEdges) = findEdges node edges
                           subtrees = map (\e -> buildTree remainingEdges $ secondNode node e) edgesFromNode
                           maxDepth = if null subtrees then 0 else (maximum $ map depth subtrees) + 1
                       in Tree node maxDepth subtrees

data Tree = Tree { root :: Node
                   , depth :: Int
                   , subtrees :: [Tree]
                  } -- deriving (Show)

instance Show Tree where
   show (Tree r d subts) = if null subts then "Tree root=" ++ (show $ r) ++ " depth=" ++ (show $ d)
                                     else "Tree root=" ++ (show $ r) ++ " depth=" ++ (show $ d) ++ "\n" ++ (unlines $ map (\e -> "\troot=" ++ (show $ root e) ++ " depth=" ++ (show $ depth e)) $ subts)

type Edge = (Int,Int)
type Node = Int

secondNode :: Node -> Edge -> Node
secondNode node edge = if fst edge == node
                       then snd edge
                       else fst edge

findEdges :: Int -> [Edge] -> ([Edge], [Edge])
findEdges node rels = partition (\(a,b) -> a == node || b == node) rels
