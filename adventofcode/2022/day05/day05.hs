import Data.Char (isSpace)
import Data.List (partition, transpose)

split :: (a -> Bool) -> [a] -> [[a]]
split p xs = f xs []
  where f [] agg = [agg]
        f (y : ys) agg
          | p y = agg : f ys []
          | otherwise = f ys (agg ++ [y])

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

data Stack = Stack String
  deriving (Show)

parseStacks :: [String] -> [Stack]
parseStacks ss = map Stack $ map (reverse . trim . (ts !!)) is
  where ts = transpose ss
        is = takeWhile (< (length ts)) [1,5..]

data Move = Move
  { count :: Int
  , from :: Int
  , to :: Int
  }
  deriving (Show)

moveFromList :: [Int] -> Move
moveFromList [a, b, c] = Move a b c

parseMove :: String -> Move
parseMove ms = moveFromList (map read ns)
  where (_, n) = partition (even . fst) $ zip [0..] $ words ms
        (_, ns) = unzip n

parseInput :: [String] -> ([Stack], [Move])
parseInput is = (parseStacks (init ss), map parseMove ms)
  where (ss:ms:_) = split (== "") is

type MoveFN = Move -> Stack -> Stack -> (Stack, Stack)

moveEach :: MoveFN
moveEach (Move { count }) (Stack fs) (Stack ts) = (Stack nfs, Stack nts)
  where n = length fs - count
        nfs = take n fs
        nts = ts ++ (reverse $ drop n fs)

moveCount :: MoveFN
moveCount (Move { count }) (Stack fs) (Stack ts) = (Stack nfs, Stack nts)
  where n = length fs - count
        nfs = take n fs
        nts = ts ++ drop n fs

moveStacks :: Move -> MoveFN -> [Stack] -> [Stack]
moveStacks m@(Move { count, from, to }) mfn is
  | from < to = bs ++ [nfs] ++ ms ++ [nts] ++ as
  | otherwise = bs ++ [nts] ++ ms ++ [nfs] ++ as
  where fs = is !! (from - 1)
        ts = is !! (to - 1)
        (nfs, nts) = mfn m fs ts
        (f, t)
          | from < to = (from, to)
          | otherwise = (to, from)
        bs = take (f - 1) is
        ms = take (t - f - 1) $ drop f is
        as = drop t is

moveAll :: MoveFN -> ([Stack], [Move]) -> [Stack]
moveAll mfn (stacks, []) = stacks
moveAll mfn (stacks, (m:ms)) = moveAll mfn (nstacks, ms)
  where nstacks = moveStacks m mfn stacks

topCrates :: [Stack] -> String
topCrates [] = ""
topCrates ((Stack s):ss) = last s : topCrates ss

part1 :: ([Stack], [Move]) -> String
part1 = topCrates . moveAll moveEach

part2 :: ([Stack], [Move]) -> String
part2 = topCrates . moveAll moveCount

main :: IO ()
main = interact $ unlines . sequence [part1, part2] . parseInput . lines
