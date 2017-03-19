import System.IO
import Control.Monad
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    input_line <- getLine
    let n = read input_line :: Int -- the number of adjacency relations

    edges <- readEdges n
    -- hPutStrLn stderr $ show edges

    let node = fst (head $ Map.toList edges)
        tree = buildTree edges node (Set.singleton node)

    hPutStrLn stderr $ show tree
    let minDepth = findMinDepth tree 0
    hPutStrLn stderr $ show minDepth
    putStrLn $ show $ fst minDepth

buildTree :: EdgeMap -> Node -> Set.Set Int -> Tree
buildTree edges node usedNodes
    = let edgesFromNode = Set.fromList (edges Map.! node)
          unusedNodes = Set.difference edgesFromNode usedNodes
          subtrees = map (\n -> buildTree edges n $ Set.union usedNodes unusedNodes) $ Set.toList unusedNodes
          maxDepth = if null subtrees then 0 else (maximum $ map depth subtrees) + 1
      in Tree node maxDepth subtrees

addEdge m e1 e2
    = let f v Nothing = Just [v]
          f v (Just vs) = Just $ v:vs
      in Map.alter (f e2) e1 m

readEdges :: Int -> IO (Map.Map Int [Int])
readEdges n
    | n == 0 = return Map.empty
    | otherwise = do
        input <- words <$> getLine
        let xi = read (input!!0) :: Int -- the ID of a person which is adjacent to yi
            yi = read (input!!1) :: Int -- the ID of a person which is adjacent to xi
        if n > 0 then do
            restOfEdges <- readEdges $ n-1
            let mapWithNewEdge = addEdge restOfEdges xi yi
            return $ addEdge mapWithNewEdge yi xi
        else
            return $ Map.fromList [(xi, [yi]), (yi, [xi])]

getSecondMaxDepth :: [Tree] -> Int -> Int
getSecondMaxDepth sts maxDepth = foldl (secondBiggest maxDepth) 0 sts

secondBiggest :: Int -> Int -> Tree -> Int
secondBiggest biggest acc s = let curDepth = depth s
                              in if curDepth > acc && curDepth < biggest
                                 then curDepth
                                 else acc


findMinDepth :: Tree -> Int -> (Int, Int)
findMinDepth tree depthOfSkippedSubtree
    | maxSubtreeCount > 1 = (dpth, root tree)
    | (dpth - 1) < skippedSubtreeDepth = (dpth, root tree)
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

data Tree = Tree { root :: Node
                   , depth :: Int
                   , subtrees :: [Tree]
                  } deriving (Show)

type EdgeMap = Map.Map Int [Int]
type Edge = (Int,Int)
type Node = Int
