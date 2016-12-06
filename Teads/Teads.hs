import System.IO
import Control.Monad
import Data.List

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    input_line <- getLine
    let n = read input_line :: Int -- the number of adjacency relations
    
    relations <- replicateM n $ do
        input_line <- getLine
        let input = words input_line
        let xi = read (input!!0) :: Int -- the ID of a person which is adjacent to yi
        let yi = read (input!!1) :: Int -- the ID of a person which is adjacent to xi
        return (xi, yi)
    
    hPutStrLn stderr $ show $ length relations
    
    let nodes = usedNodes relations 
    
    let depths = map (\n -> depth [n] relations 0) nodes
        minDepth = minimum depths
        
    -- hPutStrLn stderr $ show depths
    putStrLn $ show minDepth
    
type Relation = (Int,Int)

usedNodes :: [Relation] -> [Int]
usedNodes relations = nub . concat $ map (\(a,b) -> [a,b]) relations 

depth :: [Int] -> [Relation] -> Int -> Int
depth nodes [] d = d
depth nodes relations d = let (rels, remaining) = findRelationsForNodes nodes relations
                              newNodes = nub $ nodes ++ usedNodes rels
                          in depth newNodes remaining (d + 1)
                                        
findRelationsForNodes :: [Int] -> [Relation] -> ([Relation], [Relation])
findRelationsForNodes nodes rels = foldl folding ([], rels) nodes
                                   where folding acc n = let (used, remaining) = findRelations n rels in (used ++ (fst acc), intersect remaining (snd acc))

findRelations :: Int -> [Relation] -> ([Relation], [Relation])
findRelations node rels = partition (\(a,b) -> a == node || b == node) rels
