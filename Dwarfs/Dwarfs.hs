import System.IO
import Control.Monad
import Control.Applicative
import Data.List

readInt i = read i :: Int

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- n <- readInt <$> getLine

    -- edges <- replicateM n $ do
    --     input <- map readInt . words <$> getLine
    --     return (input !! 0, input !! 1)
    -- let edges = [(1, 2), (1, 3), (3, 4)]
    -- let edges = [(1, 2), (1, 3), (3, 4), (2, 4), (2, 5), (10, 11), (10, 1), (10, 3)]
    let edges = [(2, 3), (8, 9), (1, 2), (6, 3)]

    let (from, to) = unzip edges
        root = nub from \\ nub to
        maxDepth = maximum $ map (\r -> treeDepth edges r) root
    -- hPutStrLn stderr $ show root

    -- hPutStrLn stderr $ show tree
    putStrLn $ show $ maxDepth

treeDepth edges root = let tree = buildTree edges root
                      in depth tree + 1

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

type Edge = (Int,Int)
type Node = Int

secondNode :: Node -> Edge -> Node
secondNode node edge = if fst edge == node
                           then snd edge
                           else fst edge

findEdges :: Int -> [Edge] -> ([Edge], [Edge])
findEdges node rels = partition (\(a,b) -> a == node) rels
