import Data.List (findIndex)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Debug.Trace (trace)

data Node = Node
    { idx :: Int
    , value :: Int
    }
    deriving (Show)

parseNode :: [Int] -> M.Map Int Node
parseNode xs = M.fromList $ [(i, Node{idx = i, value = v}) | (i, v) <- zip [0 ..] xs]

deleteAt :: Int -> [a] -> (a, [a])
deleteAt i ls = (n, ns)
  where
    n = ls !! i
    ns = (take i ls) ++ (drop (i + 1) ls)

insertAt :: a -> Int -> [a] -> [a]
insertAt n i ls = (take i ls) ++ [n] ++ (drop i ls)

mix :: Int -> [(Int, Int)] -> [(Int, Int)]
mix i ns
    | i == size = ns
    | otherwise = mix (i + 1) nnns
  where
    size = length ns
    ni = fromJust $ findIndex ((== i) . fst) ns
    (n, nns) = deleteAt ni ns
    ti = (ni + (snd n)) `mod` (length nns)
    nnns = insertAt n ti nns

gcoords :: [(Int, Int)] -> Int
gcoords ns = x + y + z
  where
    size = length ns
    i = fromJust $ findIndex ((== 0) . snd) ns
    x = snd $ ns !! ((i + 1000) `mod` size)
    y = snd $ ns !! ((i + 2000) `mod` size)
    z = snd $ ns !! ((i + 3000) `mod` size)

part1 :: [Int] -> Int
part1 ns = gcoords $ mix 0 $ zip [0 ..] ns

part2 :: [Int] -> Int
part2 ns = gcoords fns
  where
    nns = zip [0 ..] $ map (* 811589153) ns
    fns = last $ take 11 $ iterate (mix 0) nns

main :: IO ()
main = interact $ unlines . map show . sequence [part1, part2] . map read . lines
