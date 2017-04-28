import System.IO
import Control.Monad
import Data.Maybe
import Data.List

r s=read s :: Int

main = do
 hSetBuffering stdout NoBuffering
 i <- words <$> getLine
 let ef = r (i!!3);ep = r (i!!4);n = r (i!!7) 
    
 es <- replicateM n $ do
  i <- words <$> getLine
  return (r (i!!0), r (i!!1))
 loop (es ++ [(ef, ep)])

loop es = do
 i <- words <$> getLine
 let cf =r (i!!0);cp = r (i!!1);
 if cf == (-1) then
  putStrLn "WAIT"
 else
  let Just (_,el) = find(\e -> fst e == cf) es in
  putStrLn$if i!!2 == "RIGHT" then if cp > el then "BLOCK" else "WAIT" else if cp < el then "BLOCK" else"WAIT"
 loop es