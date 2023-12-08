import Data.List (isSuffixOf)
import qualified Data.Map as M
import Debug.Trace (trace)

data Node = Node String
    deriving (Show, Eq, Ord)

data Pair = Pair Node Node
    deriving (Show)

parsePair :: String -> Pair
parsePair xs = Pair (Node left) (Node right)
  where
    ps = words xs
    left = init $ tail $ head ps
    right = init $ last ps

type Map = M.Map Node Pair

parseMap :: String -> Map -> Map
parseMap [] m = m
parseMap xs m = M.insert node pair m
  where
    ps = words xs
    node = Node $ head ps
    pair = parsePair $ unwords $ tail $ tail ps

parseInput :: [String] -> (String, Map)
parseInput xs = (steps, m)
  where
    steps = head xs
    rs = tail xs
    m = foldr (\r m -> parseMap r m) M.empty rs

hop :: (String, Map) -> (Node -> Bool) -> Int -> Node -> Int
hop ((x : xs), m) done count start
    | done start = count
    | x == 'L' = hop (xs ++ [x], m) done ncount left
    | otherwise = hop (xs ++ [x], m) done ncount right
  where
    ncount = count + 1
    (Just (Pair left right)) = M.lookup start m

part1 :: (String, Map) -> Int
part1 sm = hop sm done 0 (Node "AAA")
  where
    done = (== Node "ZZZ")

part2 :: (String, Map) -> Int
part2 sm@(xs, m) = foldr (\c acc -> lcm acc c) 1 counts
  where
    mkeys = M.keys m
    isStart (Node s) = isSuffixOf "A" s
    starts = filter isStart mkeys
    done (Node e) = isSuffixOf "Z" e
    counts = map (hop sm done 0) starts

main :: IO ()
main = interact $ unlines . map show . sequence [part1, part2] . parseInput . lines
