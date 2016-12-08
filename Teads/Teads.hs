import System.IO
import Control.Monad
import Data.List

main :: IO ()
main = do
    let edges = [(1, 2), (2, 3), (3, 4), (3, 7), (4, 5), (4, 6), (7, 8)] 
    
    -- hPutStrLn stderr $ show $ length relations
    
    let node = 4
                                
    hPutStrLn stderr $ show $ buildTree edges node 
    putStrLn $ show ""
 

buildTree :: [Edge] -> Node -> Tree
buildTree [] node = Tree node 0 []
buildTree edges node = let (edgesFromNode, remainingEdges) = findEdges node edges
                           subtrees = map (\e -> (e, buildTree remainingEdges $ secondNode node e)) edgesFromNode
                           maxDepth = if null subtrees then 0 else maximum $ map (depth . snd) subtrees
                       in Tree node (maxDepth + 1) subtrees 

data Tree = Tree { root :: Node
                   , depth :: Int
                   , subtrees :: [(Edge, Tree)]
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
