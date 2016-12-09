import System.IO
import Control.Monad
import Data.List

main :: IO ()
main = do
    -- let edges = [(1, 2), (2, 3), (3, 4), (3, 7), (4, 5), (4, 6), (7, 8)]
    let edges = [(0,1),(1,2),(2,3),(2,4)]
    -- let edges = [(0,1),(1,2),(1,4),(2,3),(4,5),(4,6)]
    -- hPutStrLn stderr $ show $ length relations

    let node = 0

    let tree = buildTree edges node
    hPutStrLn stderr $ "Tree: \n" ++ (show tree)

    hPutStrLn stderr $ "Second max depth: " ++ (show $ getSecondMaxDepth (subtrees tree) (depth tree)) ++ "\n"
    let depthOfSkippedSubtree = 0
    let dpth = depth tree
        maxSubtree = getSubtreesByDepth tree (dpth - 1)
        maxSubtreeCount = length maxSubtree
        subtrs = subtrees tree
        secondMaxDepth = (getSecondMaxDepth subtrs (dpth - 1))
        skippedSubtreeDepth = (max depthOfSkippedSubtree secondMaxDepth) + (if length subtrs - maxSubtreeCount == 0 then 1 else 2)
    hPutStrLn stderr $ show maxSubtree
    hPutStrLn stderr $ show secondMaxDepth
    hPutStrLn stderr $ show skippedSubtreeDepth

    hPutStrLn stderr "-----------------"
    let tree = head maxSubtree
        depthOfSkippedSubtree = 1
    let dpth = depth tree
        maxSubtree = getSubtreesByDepth tree (dpth - 1)
        maxSubtreeCount = length maxSubtree
        subtrs = subtrees tree
        secondMaxDepth = (getSecondMaxDepth subtrs (dpth - 1))
        skippedSubtreeDepth = (max depthOfSkippedSubtree secondMaxDepth) + (if length subtrs - maxSubtreeCount == 0 then 1 else 2)

    hPutStrLn stderr $ show maxSubtree
    hPutStrLn stderr $ show secondMaxDepth
    hPutStrLn stderr $ show skippedSubtreeDepth

    hPutStrLn stderr $ show $if maxSubtreeCount > 1
                             then dpth
                             else if (dpth - 1) < skippedSubtreeDepth then dpth
                                  else findMinDepth (head maxSubtree) skippedSubtreeDepth

    putStrLn $ show $ findMinDepth tree 0

getSecondMaxDepth :: [Tree] -> Int -> Int
getSecondMaxDepth sts maxDepth = foldl (secondBiggest maxDepth) 0 sts

secondBiggest :: Int -> Int -> Tree -> Int
secondBiggest biggest acc s = let curDepth = depth s
                              in if curDepth > acc && curDepth < biggest
                                 then curDepth
                                 else acc

findMinDepth :: Tree -> Int -> Int
findMinDepth tree depthOfSkippedSubtree = let dpth = depth tree
                                              maxSubtree = getSubtreesByDepth tree (dpth - 1)
                                              maxSubtreeCount = length maxSubtree
                                              subtrs = subtrees tree
                                              secondMaxDepth = (getSecondMaxDepth subtrs (dpth - 1))
                                              skippedSubtreeDepth = (max depthOfSkippedSubtree secondMaxDepth) + (if length subtrs - maxSubtreeCount == 0 then 1 else 2)
                                          in if maxSubtreeCount > 1
                                             then dpth
                                             else if (dpth - 1) < skippedSubtreeDepth then dpth
                                                  else findMinDepth (head maxSubtree) skippedSubtreeDepth

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
