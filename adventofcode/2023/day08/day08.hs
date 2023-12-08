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

follow :: (String, Map) -> Node -> (Node -> Bool) -> Int -> Int
follow ((x : xs), m) start done count
    | done start = count
    | x == 'L' = follow (xs ++ [x], m) left done ncount
    | otherwise = follow (xs ++ [x], m) right done ncount
  where
    ncount = count + 1
    (Just (Pair left right)) = M.lookup start m

part1 :: (String, Map) -> Int
part1 sm = follow sm (Node "AAA") (\n -> n == Node "ZZZ") 0

part2 :: (String, Map) -> Int
part2 sm@(xs, m) = foldr (\c acc -> lcm acc c) 1 counts
  where
    mkeys = M.keys m
    starts = filter (\(Node s) -> isSuffixOf "A" s) mkeys
    counts = map (\s -> follow sm s (\(Node e) -> isSuffixOf "Z" e) 0) starts

main :: IO ()
main = interact $ unlines . map show . sequence [part1, part2] . parseInput . lines
