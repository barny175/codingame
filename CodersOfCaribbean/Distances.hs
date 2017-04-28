import qualified Data.Map as M

type Point = (Int, Int)
type Path = [Point]

distances :: [(Int, Int)] -> [Entity] -> Point -> Int -> M.Map Entity Path -> M.Map Entity Path
distances _ [] _ _ m = m
distances [] _ _ _ m = m
distances hexes entities point n map' =
    let neighbours = map (\o -> neighbour (fst point) (snd point) o) [0..5]
        availableNeighbours = filter (\p -> p `elem` hexes) neighbours
        neighbourEntities = filter (\e -> (x e, y e) `elem` availableNeighbours) entities
        
        newMap = foldl (\ne -> if ) map' neighbourEntities
        restEntities = filter (\e -> e `notElem` neighbourEntities) entities
        availableHexes = filter (\p -> p `notElem` availableNeighbours) $ delete ent hexes
        dists n = map (\(e, p) -> (e, n : p)) $ distances availableHexes restEntities n
        furtherDists = concat $ map dists availableNeighbours
    in (map (\e -> (e, [(x e, y e)])) neighbourEntities) ++ furtherDists


updateMap map' entities n =
    let alterMap = M.
    in foldl (\m e -> alterMap m e) map' entities
