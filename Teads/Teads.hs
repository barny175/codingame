import System.IO
import Control.Monad
import Data.List
import qualified Data.Map as Map

edges = [(0, 1), (1, 2), (2, 3), (2, 4)]

main :: IO ()
main = do
    input_line <- getLine
    let n = read input_line :: Int -- the number of adjacency relations
    let readInt i = read i ::Int

    -- edges <- replicateM n $ do
    --     input <- map (readInt).words <$> getLine
    --     return (input!!0, input!!1)

    -- hPutStrLn stderr $ show $ length relations

    let tree = buildMap edges

    putStrLn $ show $ "result"

newPath (e1, e2) (f1, f2)
    | e1 == f1  = (e2, f2)
    | e1 == f2  = (e2, f1)
    | e2 == f1  = (e1, f2)
    | e2 == f2  = (e1, f1)

insertEdge e@(e1, e2) m =
    let toInsert = Map.filterWithKey (\(f1, f2) k -> e1 == f1 || e2 == f1 || e1 == f2 || e2 == f2) m
        insert k v m = Map.insert (newPath k e) (v+1) m
        newMap = Map.insert e 1 m
    in Map.foldrWithKey insert newMap toInsert

buildMap edges = foldr insertEdge Map.empty edges
