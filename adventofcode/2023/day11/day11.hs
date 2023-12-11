import Data.List (intersect, tails, transpose)
import Debug.Trace (trace)

isSpace :: Char -> Bool
isSpace = (== '.')

isGalaxy :: Char -> Bool
isGalaxy = (== '#')

isEmptyRow :: String -> Bool
isEmptyRow = all isSpace

data Pos = Pos Int Int
    deriving (Show, Eq)

mDist :: Pos -> Pos -> Int
mDist (Pos x1 y1) (Pos x2 y2) = abs (x1 - x2) + abs (y1 - y2)

type Galaxy = [String]

emptyRowIndices :: Galaxy -> [Int]
emptyRowIndices = map fst . filter ((== True) . snd) . zip [0 ..] . map isEmptyRow

countSpaceBetween :: Int -> Int -> [Int] -> Int
countSpaceBetween a b r
    | a < b = length $ intersect [a .. b] r
    | otherwise = length $ intersect [b .. a] r

dist :: Pos -> Pos -> [Int] -> [Int] -> Int -> Int
dist (Pos x1 y1) (Pos x2 y2) rs cs m = d + (ex + ey) * (m - 1)
  where
    ex = countSpaceBetween x1 x2 cs
    ey = countSpaceBetween y1 y2 rs
    d = mDist (Pos x1 y1) (Pos x2 y2)

sumDists :: [Pos] -> [Int] -> [Int] -> Int -> Int
sumDists gps rs cs m = sum [dist p1 p2 rs cs m | (p1 : ps) <- tails gps, p2 <- ps]

ymax :: Galaxy -> Int
ymax = (subtract 1) . length

xmax :: Galaxy -> Int
xmax = (subtract 1) . length . (!! 0)

atPos :: Pos -> Galaxy -> Char
atPos (Pos x y) = (!! x) . (!! y)

galaxySumDists :: Int -> Galaxy -> Int
galaxySumDists m galaxy = sumDists gps rs cs m
  where
    rs = emptyRowIndices galaxy
    cs = emptyRowIndices (transpose galaxy)
    gps = [Pos x y | y <- [0 .. (ymax galaxy)], x <- [0 .. (xmax galaxy)], isGalaxy $ atPos (Pos x y) galaxy]

part1 :: Galaxy -> Int
part1 = galaxySumDists 2

part2 :: Galaxy -> Int
part2 = galaxySumDists 1000000

main :: IO ()
main = interact $ unlines . map show . sequence [part1, part2] . lines
