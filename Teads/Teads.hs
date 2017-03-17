import System.IO
import Control.Monad
import Data.List
import qualified Data.Map as Map

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    input_line <- getLine
    let n = read input_line :: Int -- the number of adjacency relations

    edges <- readEdges n
    hPutStrLn stderr $ show edges

    -- let tree = buildTree edges (fst $ head edges)

    -- putStrLn $ show $ findMinDepth tree 0

readEdges :: Int -> IO (Map.Map Int Int)
readEdges n
    | n == 0 = return Map.empty
    | otherwise = do
        input <- words <$> getLine
        let xi = read (input!!0) :: Int -- the ID of a person which is adjacent to yi
            yi = read (input!!1) :: Int -- the ID of a person which is adjacent to xi
            newEdges = Map.fromList [(xi, yi), (yi, xi)]
        if n > 0 then do
            restOfEdges <- readEdges $ n-1
            return $ Map.union newEdges restOfEdges
        else
            return newEdges

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
