import qualified Data.Map as M

data Direction = LEFT | UP | RIGHT | DOWN
  deriving (Show)

parseDirection :: Char -> Direction
parseDirection 'U' = UP
parseDirection 'L' = LEFT
parseDirection 'D' = DOWN
parseDirection 'R' = RIGHT

data Move = Move Direction Int
  deriving (Show)

parseMove :: String -> Move
parseMove (d:ds) = Move (parseDirection d) (read ds)

data Point = Point Int Int
  deriving (Show, Eq, Ord)

dist :: Point -> Point -> Int
dist (Point x1 y1) (Point x2 y2) = maximum [abs (x1 - x2), abs (y1 - y2)]

data Rope = Rope [Point]
  deriving (Show)

moveHead :: Direction -> Point -> Point
moveHead LEFT (Point x y) = Point (x-1) y
moveHead RIGHT (Point x y) = Point (x+1) y
moveHead UP (Point x y) = Point x (y-1)
moveHead DOWN (Point x y) = Point x (y+1)

moveTail :: Point -> Point -> Point
moveTail head@(Point x1 y1) tail@(Point x2 y2)
  | dist head tail <= 1 = tail
  | otherwise = Point nx2 ny2
  where nx2
          | x1 > x2 = x2 + 1
          | x1 < x2 = x2 - 1
          | otherwise = x2
        ny2
          | y1 > y2 = y2 + 1
          | y1 < y2 = y2 - 1
          | otherwise = y2

moveOne :: Rope -> Maybe Point -> Direction -> M.Map Point Bool -> Rope -> (Rope, M.Map Point Bool)
moveOne (Rope [t]) (Just h) dirn hist (Rope ps) = (Rope (ps ++ [nt]), M.insert nt True hist)
  where nt = moveTail h t
moveOne (Rope (h:ps)) Nothing dirn hist (Rope []) = moveOne (Rope ps) (Just nh) dirn hist (Rope [nh])
  where nh = moveHead dirn h
moveOne (Rope (t:ps)) (Just h) dirn hist (Rope rs) = moveOne (Rope ps) (Just nt) dirn hist (Rope (rs ++ [nt]))
  where nt = moveTail h t

move :: Rope -> Move -> M.Map Point Bool -> (Rope, M.Map Point Bool)
move rope (Move dirn 0) hist = (rope, hist)
move rope (Move dirn count) hist = move nrope (Move dirn (count - 1)) nhist
  where (nrope, nhist) = moveOne rope Nothing dirn hist (Rope [])

moveAll :: Rope -> M.Map Point Bool -> [Move] -> Int
moveAll _ hist [] = M.size hist
moveAll rope hist (m:ms) = moveAll nrope nhist ms
  where (nrope, nhist) = move rope m hist

part1 :: [Move] -> Int
part1 = moveAll rope M.empty
  where rope = Rope [ Point 0 0
                    , Point 0 0
                    ]

part2 :: [Move] -> Int
part2 = moveAll rope M.empty
  where rope = Rope [ Point 0 0
                    , Point 0 0
                    , Point 0 0
                    , Point 0 0
                    , Point 0 0
                    , Point 0 0
                    , Point 0 0
                    , Point 0 0
                    , Point 0 0
                    , Point 0 0
                    ]

main :: IO ()
main = interact $ unlines . map show . sequence [part1, part2] . map parseMove . lines
