import           Data.List
import           Debug.Trace
import           System.IO.Unsafe
import qualified Data.Map.Strict as M

data Pos = Pos Int Int
  deriving (Eq, Ord, Show)

data Direction = West
                | East
                | Northwest
                | Southeast
                | North
                | South
                | Northeast
                | Southwest

allDirections :: [Direction]
allDirections = [ West
                , East
                , Northwest
                , Southeast
                , North
                , South
                , Northeast
                , Southwest
                ]

findObstacle :: Direction -> Pos -> [Pos] -> Maybe Pos
findObstacle _ _ [] = Nothing
findObstacle West (Pos x y) os
  | null wos = Nothing
  | otherwise = Just (last wos)
  where wos = sort $ filter (\(Pos ox oy) -> y == oy && x > ox) os
findObstacle East (Pos x y) os
  | null wos = Nothing
  | otherwise = Just (head wos)
  where wos = sort $ filter (\(Pos ox oy) -> y == oy && x < ox) os
findObstacle North (Pos x y) os
  | null wos = Nothing
  | otherwise = Just (head wos)
    where wos = sort $ filter (\(Pos ox oy) -> x == ox && y < oy) os
findObstacle South (Pos x y) os
  | null wos = Nothing
  | otherwise = Just (last wos)
    where wos = sort $ filter (\(Pos ox oy) -> x == ox && y > oy) os
findObstacle Northeast (Pos x y) os
  | null wos = Nothing
  | otherwise = Just (head wos)
    where wos = sort $ filter (\(Pos ox oy) -> x - ox == y - oy && x < ox && y < oy) os
findObstacle Southwest (Pos x y) os
  | null wos = Nothing
  | otherwise = Just (last wos)
    where wos = sort $ filter (\(Pos ox oy) -> x - ox == y - oy && x > ox && y > oy) os
findObstacle Northwest (Pos x y) os
  | null wos = Nothing
  | otherwise = Just (last wos)
    where wos = sort $ filter (\(Pos ox oy) -> abs (x - ox) == abs (y - oy) && x > ox && y < oy) os
findObstacle Southeast (Pos x y) os
  | null wos = Nothing
  | otherwise = Just (head wos)
    where wos = sort $ filter (\(Pos ox oy) -> abs (x - ox) == abs (y - oy) && x < ox && y > oy) os

data Square = Empty | Obstacle | Queen
  deriving (Eq, Show)

printSquare Empty = "_"
printSquare Queen = "Q"
printSquare Obstacle = "x"

data Board = Board Int (M.Map Pos Square)

printBoard :: Board -> String
printBoard (Board n b) = unlines $ map (unwords . map f) ps
  where ps = [[Pos x y | x <- [1..n]] | y <- reverse [1..n]]
        f p = maybe "_" printSquare $ M.lookup p b

isInside :: Board -> Pos -> Bool
isInside (Board n b) (Pos x y)
  | x < 1 && x > n = False
  | otherwise = y >= 1 && y <= n

isOutside :: Board -> Pos -> Bool
isOutside = (.) not . isInside

insertObstacle :: Pos -> Board -> Board
insertObstacle p (Board n b) = Board n $ M.insert p Obstacle b

parseObstaclePositions :: [Int] -> [Pos]
parseObstaclePositions [] = []
parseObstaclePositions (r:c:rs) = opos : parseObstaclePositions rs
  where opos = Pos c r

insertQueen :: Pos -> Board -> Board
insertQueen p (Board n b) = Board n $ M.insert p Queen b

isEmpty :: Board -> Pos -> Bool
isEmpty (Board _ b) p = case M.lookup p b of
                          (Just Empty) -> True
                          _ -> False

isObstacle :: Board -> Pos -> Bool
isObstacle (Board _ b) p = case M.lookup p b of
                             (Just Obstacle) -> True
                             _ -> False

isQueen :: Board -> Pos -> Bool
isQueen (Board _ b) p = case M.lookup p b of
                          (Just Queen) -> True
                          _ -> False

distEdge :: Int -> Pos -> Direction -> Int
distEdge _ (Pos x _) West  = x - 1
distEdge n (Pos x _) East  = n - x
distEdge _ (Pos _ y) South = y - 1
distEdge n (Pos _ y) North = n - y
distEdge n p Northeast     = minimum $ map (distEdge n p) [North, East]
distEdge n p Northwest     = minimum $ map (distEdge n p) [North, West]
distEdge n p Southeast     = minimum $ map (distEdge n p) [South, East]
distEdge n p Southwest     = minimum $ map (distEdge n p) [South, West]

distPos :: Pos -> Pos -> Direction -> Int
distPos (Pos x y) (Pos ox oy) West = x - ox - 1
distPos (Pos x y) (Pos ox oy) East = ox - x - 1
distPos (Pos x y) (Pos ox oy) South = y - oy - 1
distPos (Pos x y) (Pos ox oy) North = oy - y - 1
distPos (Pos x y) (Pos ox oy) Northeast = oy - y - 1
distPos (Pos x y) (Pos ox oy) Northwest = oy - y - 1
distPos (Pos x y) (Pos ox oy) Southeast = y - oy - 1
distPos (Pos x y) (Pos ox oy) Southwest = y - oy - 1

attackCount :: Board -> Pos -> [Pos] -> Direction -> Int
attackCount (Board n b) p@(Pos x y) os d = maybe de dp obst
    where obst = findObstacle d p os
          de = distEdge n p d
          dp pos = distPos p pos d

attackCounts :: Pos -> [Direction] -> Board -> [Pos] -> Int
attackCounts p ds b os = foldl f 0 ds
  where f acc d = acc + attackCount b p os d

totalAttackCount :: [Int] -> Int
totalAttackCount (n:_:rq:cq:os) = attackCounts qpos allDirections board ops
  where qpos = Pos cq rq
        ops = parseObstaclePositions os
        board = foldl (flip insertObstacle) (insertQueen qpos $ Board n M.empty) ops

solve :: [Int] -> Int
solve = totalAttackCount

main :: IO ()
main = interact $ show . solve . map read . words
