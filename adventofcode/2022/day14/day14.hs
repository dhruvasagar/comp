import Data.List (isPrefixOf)
import qualified Data.Map as M
import Debug.Trace (trace)
import Data.Maybe (fromJust)

split :: Eq a => [a] -> [a] -> [[a]]
split x y = func x y [[]]
    where
        func x [] z = reverse $ map (reverse) z
        func x (y:ys) (z:zs) = if (take (length x) (y:ys)) == x then
            func x (drop (length x) (y:ys)) ([]:(z:zs))
        else
            func x ys ((y:z):zs)

eachPair :: Eq a => [a] -> [(a, a)]
eachPair [a,b] = [(a,b)]
eachPair cs = (a, b) : eachPair (tail cs)
  where [a, b] = take 2 cs

data Point = Point Int Int
  deriving (Show, Eq, Ord)

parsePoint :: String -> Point
parsePoint ps = Point x y
  where [x, y] = map read $ split "," ps

allPoints :: Point -> Point -> [Point]
allPoints (Point x1 y1) (Point x2 y2)
  | x1 == x2 = fy y1 y2
  | otherwise = fx x1 x2
  where fy y1 y2
          | y1 < y2 = [Point x1 yi | yi <- [y1..y2]]
          | otherwise = [Point x1 yi | yi <- [y2..y1]]
        fx x1 x2
          | x1 < x2 = [Point xi y1 | xi <- [x1..x2]]
          | otherwise = [Point xi y1 | xi <- [x2..x1]]

below :: Point -> Point
below (Point x y) = Point x (y + 1)

left :: Point -> Point
left (Point x y) = Point (x - 1) y

right :: Point -> Point
right (Point x y) = Point (x + 1) y

parsePath :: M.Map Point Char -> String -> M.Map Point Char
parsePath mpc ps = foldl (\m p -> M.insert p '#' m) mpc nps
  where cs = map parsePoint $ split " -> " ps
        nps = concat $ map (uncurry allPoints) $ eachPair cs

parsePaths :: [String] -> M.Map Point Char
parsePaths ps = foldl (parsePath ) M.empty ps

isFallingThrough :: M.Map Point Char -> Point -> Bool
isFallingThrough m (Point x y) = y > ymax
  where ymax = maximum $ map (\(Point _ y) -> y) $ M.keys m

isNotWall :: M.Map Point Char -> Point -> Bool
isNotWall m p = M.notMember p m

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = (take n xs) : (chunksOf n (drop n xs))

printMap :: M.Map Point Char -> String
printMap m = unlines $ chunksOf (xmax - xmin + 1) $ map f ps
  where ks = M.keys m
        xmin = minimum $ map (\(Point x _) -> x) ks
        xmax = maximum $ map (\(Point x _) -> x) ks
        ymin = minimum $ map (\(Point _ y) -> y) ks
        ymax = maximum $ map (\(Point _ y) -> y) ks
        ps = [Point x y | y <- [0..(ymax + 2)], x <- [xmin..xmax]]
        f :: Point -> Char
        f p
          | p == (Point 500 0) = '+'
          | otherwise = maybe '.' id (M.lookup p m)

simulateSand :: M.Map Point Char -> Point -> Int
simulateSand m p
  | isFallingThrough m p = length $ M.filter (== 'o') m
  | isNotWall m np = simulateSand m np
  | isNotWall m (left np) = simulateSand m (left np)
  | isNotWall m (right np) = simulateSand m (right np)
  | otherwise = simulateSand nm (Point 500 0)
    where np = below p
          nm = M.insert p 'o' m

part1 :: M.Map Point Char -> Int
part1 m = simulateSand m (Point 500 0)

isNotWallOrFloor :: M.Map Point Char -> Int -> Point -> Bool
isNotWallOrFloor m ymax p@(Point x y)
  | y == ymax = False
  | otherwise = isNotWall m p

printMapWithFloor :: M.Map Point Char -> Int -> String
printMapWithFloor m fy = unlines $ chunksOf (xmax - xmin + 1) $ map f ps
  where ks = M.keys m
        xmin = minimum $ map (\(Point x _) -> x) ks
        xmax = maximum $ map (\(Point x _) -> x) ks
        ymin = minimum $ map (\(Point _ y) -> y) ks
        ymax = maximum $ map (\(Point _ y) -> y) ks
        ps = [Point x y | y <- [0..(ymax + 2)], x <- [xmin..xmax]]
        f :: Point -> Char
        f p@(Point _ y)
          | y == fy = '#'
          | p == (Point 500 0) = '+'
          | otherwise = maybe '.' id (M.lookup p m)

simulateSandWithFloor :: M.Map Point Char -> Int -> Point -> Int
simulateSandWithFloor m ymax p
  | M.member top m && ((== 'o') $ fromJust $ M.lookup top m) = length $ M.filter (== 'o') m
  | isNotWallOrFloor m ymax np = simulateSandWithFloor m ymax np
  | isNotWallOrFloor m ymax (left np) = simulateSandWithFloor m ymax (left np)
  | isNotWallOrFloor m ymax (right np) = simulateSandWithFloor m ymax (right np)
  | otherwise = simulateSandWithFloor nm ymax top
  where top = Point 500 0
        np = below p
        nm = M.insert p 'o' m

part2 :: M.Map Point Char -> Int
part2 m = simulateSandWithFloor m (ymax + 2) (Point 500 0)
  where ymax = maximum $ map (\(Point _ y) -> y) $ M.keys m

main :: IO ()
main = interact $ unlines . map show . sequence [part1, part2] . parsePaths . lines
