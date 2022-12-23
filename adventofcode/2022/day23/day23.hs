import Data.List (sort)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Debug.Trace (trace)

data Point = Point Int Int
    deriving (Show, Eq, Ord)

data Value
    = Elf
    | Empty
    deriving (Eq)

instance Show Value where
    show Elf = "#"
    show Empty = "."

parseValue :: Char -> Value
parseValue '.' = Empty
parseValue '#' = Elf

type Grid = M.Map Point Value

bounds :: Grid -> (Point, Point)
bounds grid = (Point xmin ymin, Point xmax ymax)
  where
    xmin = minimum $ map (\(Point x _) -> x) $ M.keys $ M.filter (== Elf) grid
    xmax = maximum $ map (\(Point x _) -> x) $ M.keys $ M.filter (== Elf) grid
    ymin = minimum $ map (\(Point _ y) -> y) $ M.keys $ M.filter (== Elf) grid
    ymax = maximum $ map (\(Point _ y) -> y) $ M.keys $ M.filter (== Elf) grid

showGrid :: Grid -> String
showGrid grid = unlines $ map (foldl (\s p -> s ++ (show $ fromMaybe Empty $ M.lookup p grid)) "") ps
  where
    (Point xmin ymin, Point xmax ymax) = bounds grid
    ps = [[Point x y | x <- [xmin .. xmax]] | y <- [ymin .. ymax]]

countEmpty :: Grid -> Int
countEmpty grid = length $ filter (\p -> isEmpty $ M.lookup p grid) ps
  where
    (Point xmin ymin, Point xmax ymax) = bounds grid
    ps = [Point x y | x <- [xmin .. xmax], y <- [ymin .. ymax]]

parseGrid :: [String] -> Grid
parseGrid ls = M.fromList $ zip ps (concat vs)
  where
    vs = init $ map (map parseValue) ls
    ysize = length vs
    xsize = length $ head vs
    ps = [Point x y | y <- [0 .. (ysize - 1)], x <- [0 .. (xsize - 1)]]

neighbors :: Point -> [Point]
neighbors (Point x y) =
    [ Point (x - 1) (y - 1)
    , Point x (y - 1)
    , Point (x + 1) (y - 1)
    , Point (x - 1) y
    , Point (x + 1) y
    , Point (x - 1) (y + 1)
    , Point x (y + 1)
    , Point (x + 1) (y + 1)
    ]

northNeighbors :: Point -> [Point]
northNeighbors (Point x y) =
    [ Point (x - 1) (y - 1)
    , Point x (y - 1)
    , Point (x + 1) (y - 1)
    ]

southNeighbors :: Point -> [Point]
southNeighbors (Point x y) =
    [ Point (x - 1) (y + 1)
    , Point x (y + 1)
    , Point (x + 1) (y + 1)
    ]

westNeighbors :: Point -> [Point]
westNeighbors (Point x y) =
    [ Point (x - 1) (y - 1)
    , Point (x - 1) y
    , Point (x - 1) (y + 1)
    ]

eastNeighbors :: Point -> [Point]
eastNeighbors (Point x y) =
    [ Point (x + 1) (y - 1)
    , Point (x + 1) y
    , Point (x + 1) (y + 1)
    ]
isEmpty :: Maybe Value -> Bool
isEmpty (Just Elf) = False
isEmpty _ = True

isElf :: Maybe Value -> Bool
isElf (Just Elf) = True
isElf _ = False

none :: Grid -> [Point] -> Bool
none grid = all isEmpty . map (\p -> M.lookup p grid)

move :: Grid -> Point -> Point -> Grid
move grid p1 p2 = nngrid
  where
    ngrid = M.insert p1 Empty grid
    nngrid = M.insert p2 Elf ngrid

data Direction
    = North
    | South
    | West
    | East
    deriving (Show)

moveDirection :: Grid -> Int -> Point -> (Point, Point)
moveDirection grid n p@(Point x y)
    | length nns == 0 = (p, p)
    | otherwise = np $ fst $ head $ nns
  where
    dns =
        [ (North, northNeighbors p)
        , (South, southNeighbors p)
        , (West, westNeighbors p)
        , (East, eastNeighbors p)
        ]
    ns = take 4 $ drop (mod n 4) $ cycle dns
    np :: Direction -> (Point, Point)
    np North = (p, Point x (y - 1))
    np South = (p, Point x (y + 1))
    np West = (p, Point (x - 1) y)
    np East = (p, Point (x + 1) y)
    nns = filter (\(d, nss) -> none grid nss) ns

firstHalfEach :: Grid -> Int -> Point -> (Point, Point)
firstHalfEach grid n p
    | none grid $ ns = (p, p)
    | otherwise = moveDirection grid n p
  where
    ns = neighbors p

firstHalf :: Grid -> Int -> [(Point, Point)]
firstHalf grid n = map (firstHalfEach grid n) $ M.keys $ M.filter (== Elf) grid

secondHalf :: Grid -> [(Point, Point)] -> Grid
secondHalf grid ps = foldl (\g (p1, p2) -> np g p1 p2) grid ps
  where
    np :: Grid -> Point -> Point -> Grid
    np grid p1 p2
        | elem p2 tps = grid
        | otherwise = move grid p1 p2
      where
        tps = map snd $ filter ((/= p1) . fst) ps

playRound :: Grid -> Int -> Grid
playRound grid n = secondHalf grid $ firstHalf grid n

play :: Grid -> Int -> Int -> (Int, Grid)
play grid n nmax
    | n == nmax || grid == ngrid = (n, grid)
    | otherwise = play ngrid (n + 1) nmax
  where
    ngrid = playRound grid (trace ("round: " ++ show n) n)

part1 :: Grid -> Int
part1 grid = countEmpty $ snd $ play grid 0 10

part2 :: Grid -> Int
part2 grid = (+ 1) $ fst $ play grid 0 (-1)

main :: IO ()
main = interact $ unlines . map show . sequence [part1, part2] . parseGrid . lines
