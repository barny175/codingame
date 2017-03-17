import System.IO
import Control.Monad
import Data.List

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    input_line <- getLine
    let n = read input_line :: Int -- the number of adjacency relations
    
    edges <- replicateM n $ do
        input_line <- getLine
        let input = words input_line
        let xi = read (input!!0) :: Int -- the ID of a person which is adjacent to yi
        let yi = read (input!!1) :: Int -- the ID of a person which is adjacent to xi
        return (xi, yi)
    
    -- hPutStrLn stderr $ show $ length edges
    
    let tree = buildTree edges (fst $ head edges)
    
    -- hPutStrLn stderr $ show $ tree
    -- hPutStrLn stderr $ debug
    putStrLn $ show $ findMinDepth tree 0

getSecondMaxDepth :: [Tree] -> Int -> Int
getSecondMaxDepth sts maxDepth = foldl (secondBiggest maxDepth) 0 sts

secondBiggest :: Int -> Int -> Tree -> Int
secondBiggest biggest acc s = let curDepth = depth s
                              in if curDepth > acc && curDepth < biggest
                                 then curDepth
                                 else acc


findMinDepth :: Tree -> Int -> Int
findMinDepth tree depthOfSkippedSubtree
    | maxSubtreeCount > 1 = dpth
    | (dpth - 1) < skippedSubtreeDepth = dpth
    | otherwise = findMinDepth (head maxSubtree) skippedSubtreeDepth
    where
        dpth = depth tree
        maxSubtree = getSubtreesByDepth tree (dpth - 1)
        maxSubtreeCount = length maxSubtree
        subtrs = subtrees tree
        secondMaxDepth = (getSecondMaxDepth subtrs (dpth - 1))
        skippedSubtreeDepth = (max depthOfSkippedSubtree (secondMaxDepth + 1)) + 1

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
                  } deriving (Show)

-- instance Show Tree where
--    show (Tree root depth subtrees) = if null subtrees then "Tree root=" ++ (show $ root) ++ " depth=" ++ (show $ depth)
--                                      else "Tree root=" ++ (show $ root) ++ " depth=" ++ (show $ depth) ++ "\n" ++ (unlines $ map (\e -> "\t" ++ (show e)) $ subtrees)

type Edge = (Int,Int)
type Node = Int

secondNode :: Node -> Edge -> Node
secondNode node edge = if fst edge == node
                       then snd edge
                       else fst edge

findEdges :: Int -> [Edge] -> ([Edge], [Edge])
findEdges node rels = partition (\(a,b) -> a == node || b == node) rels
