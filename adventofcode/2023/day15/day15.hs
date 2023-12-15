import Data.Char (isSpace, ord)
import Data.List (findIndex)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace (trace)

rstrip = reverse . dropWhile isSpace . reverse

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delimiter = foldr f [[]]
  where
    f c l@(x : xs)
        | c == delimiter = [] : l
        | otherwise = (c : x) : xs

hash :: String -> Int
hash = foldl f 0
  where
    f s c = mod ((s + ord c) * 17) 256

type HashMap = M.Map Int [(String, Int)]

delete :: String -> [(String, Int)] -> [(String, Int)]
delete l = filter ((/= l) . fst)

delOp :: String -> HashMap -> HashMap
delOp l m
    | M.member index m = M.insert index (delete l boxes) m
    | otherwise = m
  where
    index = hash l
    boxes = fromJust $ M.lookup index m

update :: (String, Int) -> [(String, Int)] -> [(String, Int)]
update (l, f) bs = case findIndex ((== l) . fst) bs of
    Nothing -> bs ++ [(l, f)]
    (Just lindex) -> (take lindex bs) ++ [(l, f)] ++ (drop (lindex + 1) bs)

setOp :: (String, Int) -> HashMap -> HashMap
setOp (l, f) m = M.insert index (update (l, f) boxes) m
  where
    index = hash l
    boxes = fromMaybe [] $ M.lookup index m

power :: [(String, Int)] -> Int
power bs = foldl (\c (i, (_, f)) -> c + (i * f)) 0 $ zip [1 ..] bs

hashmap :: HashMap -> [String] -> Int
hashmap m [] = M.foldlWithKey (\t i bs -> t + ((i + 1) * (power bs))) 0 m
hashmap m (o : os)
    | elem '-' o = hashmap (delOp (head $ splitBy '-' o) m) os
    | elem '=' o = hashmap (setOp (l, read f) m) os
  where
    (l : f : _) = splitBy '=' o

part1 :: [String] -> Int
part1 = sum . map hash

part2 :: [String] -> Int
part2 = hashmap M.empty

main :: IO ()
main = interact $ unlines . map show . sequence [part1, part2] . (splitBy ',') . rstrip
