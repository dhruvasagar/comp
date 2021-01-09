import Data.List

seatBits :: String -> [Int]
seatBits [] = []
seatBits (x:xs) = b x : seatBits xs
  where b :: Char -> Int
        b 'B' = 1
        b 'F' = 0
        b 'R' = 1
        b 'L' = 0
        b _ = 0

seatID :: [Int] -> Int
seatID bs = foldl f 0 bis
  where bis = zip (reverse bs) [0..]
        f :: Int -> (Int, Int) -> Int
        f r (x, y) = r + 2^y * x

part1 :: [String] -> Int
part1 = maximum . map (seatID . seatBits)

part2 :: [String] -> Int
part2 ls = r
  where ss = map (seatID . seatBits) ls
        r:_ = [(minimum ss)..(maximum ss)] \\ ss

solve :: [String] -> [Int]
solve ls = [part1 ls, part2 ls]

main :: IO ()
main = interact $ unlines . map show . solve . lines
