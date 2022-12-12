import Data.List (isPrefixOf, sortBy)

split :: (a -> Bool) -> [a] -> [[a]]
split p xs = f xs []
  where f [] agg = [agg]
        f (y : ys) agg
          | p y = agg : f ys []
          | otherwise = f ys (agg ++ [y])

data Monkey = Monkey
  { index :: Int
  , items :: [Int]
  , operation :: (Int -> Int -> Int)
  , testn :: Int
  , tidx :: Int
  , fidx :: Int
  , total_inspected :: Int
  }

instance Show Monkey where
  show (Monkey { index, items, testn, tidx, fidx, total_inspected }) =
    "Monkey: " ++ show index ++ " items: " ++ show items ++
      " testn: " ++ show testn ++ " tidx: " ++ show tidx ++ " fidx: " ++
        show fidx ++ " total_inspected: " ++ show total_inspected ++ "\n"

parseIndex :: String -> Int
parseIndex = read . last . words . head . split (== ':')

parseItems :: String -> [Int]
parseItems = map read . split (== ',') . last . split (== ':')

square :: Int -> Int
square x = x * x

parseOperation :: String -> (Int -> Int -> Int)
parseOperation os
  | op == " old * old" = (mod . square)
  | isPrefixOf " old *" op = (mod . (* opa))
  | otherwise = (mod . (+ opa))
    where op = last $ split (== '=') os
          opa = read $ last $ words op

parseTest :: [String] -> (Int, Int, Int)
parseTest [l0, l1, l2] = (testn, tidx, fidx)
  where tidx = read $ last $ words l1
        fidx = read $ last $ words l2
        testn = read $ last $ words l0

parseMonkey :: [String] -> Monkey
parseMonkey [m0,m1,m2,m3,m4,m5] = Monkey index items operation testn tidx fidx 0
  where index = parseIndex m0
        items = parseItems m1
        operation = parseOperation m2
        (testn, tidx, fidx) = parseTest [m3,m4,m5]

test :: Monkey -> Int -> Int
test (Monkey { testn, tidx, fidx }) i
  | ((== 0) . (`mod` testn)) i = tidx
  | otherwise = fidx

type PlayFN = Monkey -> Int -> [(Int, Int)] -> (Monkey, [(Int, Int)])

play :: PlayFN
play m@(Monkey { items = [] }) _ nt = (m, nt)
play m@(Monkey
  { items = (i:is)
  , operation
  , total_inspected
  }) base nt = play nmonkey base nnt
  where nitem = (div (operation i base) 3)
        nidx = test m nitem
        nnt = nt ++ [(nitem, nidx)]
        nti = total_inspected + 1
        nmonkey = m { items = is, total_inspected = nti }

play2 :: PlayFN
play2 m@(Monkey { items = [] }) _ nt = (m, nt)
play2 m@(Monkey
  { items = (i:is)
  , operation
  , total_inspected
  }) base nt = play2 nmonkey base nnt
  where nitem = operation i base
        nidx = test m nitem
        nnt = nt ++ [(nitem, nidx)]
        nti = total_inspected + 1
        nmonkey = m { items = is, total_inspected = nti }

throwItem :: [Monkey] -> (Int, Int) -> [Monkey]
throwItem ms (nitem, nidx) = (take nidx ms) ++ [nm] ++ (drop (nidx+1) ms)
  where m = ms !! nidx
        nm = m { items = (items m ++ [nitem]) }

throwItems :: [Monkey] -> [(Int, Int)] -> [Monkey]
throwItems ms [] = ms
throwItems ms (n:ns) = throwItems nms ns
  where nms = throwItem ms n

playRound :: PlayFN -> Int -> [Monkey] -> Int -> [Monkey]
playRound playf base ms idx
  | idx == (length ms) = ms
  | otherwise = playRound playf base fms (idx + 1)
  where m = ms !! idx
        (nm, nis) = playf m base []
        nms = throwItems ms nis
        fms = (take idx nms) ++ [nm] ++ (drop (idx+1) nms)

playRounds :: PlayFN -> Int -> Int -> [Monkey] -> [Monkey]
playRounds playf base 0 ms = ms
playRounds playf base n ms = playRounds playf base (n-1) nms
  where nms = playRound playf base ms 0

revSort = sortBy (flip compare)

top2Active :: [Monkey] -> Int
top2Active = product . take 2 . revSort . map total_inspected

part1 :: [Monkey] -> Int
part1 ms = top2Active $ playRounds play base 20 ms
  where base = product $ map testn ms

part2 :: [Monkey] -> Int
part2 ms = top2Active $ playRounds play2 base 10000 ms
  where base = product $ map testn ms

main :: IO ()
main = interact $ unlines . map show . sequence [part1, part2] . map parseMonkey . split (== "") . lines
