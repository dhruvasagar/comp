{-# LANGUAGE TupleSections #-}

import Data.List
import Debug.Trace (trace)
import qualified Data.Map as M

data Point = Point Int Int
  deriving (Eq, Ord, Show)

newtype Inst = Inst Point
  deriving Show

splitLines :: [String] -> String -> [[String]]
splitLines [] _ = []
splitLines lines pat
  | null rs = [ss]
  | otherwise = ss : splitLines rs pat
  where ss = takeWhile (/= pat) lines
        rs = dropWhile (== pat) $ dropWhile (/= pat) lines

splitStr :: Char -> String -> [String]
splitStr _ [] = []
splitStr ch cs
  | null rs = [ss]
  | otherwise = ss : splitStr ch rs
  where ss = takeWhile (/= ch) cs
        rs = dropWhile (== ch) $ dropWhile (/= ch) cs

parseCoords :: [String] -> [Point]
parseCoords cs = ps
  where f = splitStr ','
        ps = map ((\[x, y] -> Point x y) . (map read . f)) cs

parseInsts :: [String] -> [Inst]
parseInsts cs = ps
  where f = splitStr '='
        p :: [String] -> Point
        p ["x", n] = Point (read n) 0
        p ["y", n] = Point 0 (read n)
        p [_] = Point 0 0
        ps = map (Inst . p . f . last . words) cs

fld :: [Point] -> Inst -> [Point]
fld ps i = fs
  where (Inst (Point x y)) = i
        f :: Int -> Int -> Int
        f cx x
          | x == 0 = cx
          | otherwise = x - abs (x - cx)
        fs = map (\(Point cx cy) -> Point (f cx x) (f cy y)) ps

part1 :: [Point] -> Inst -> Int
part1 ps i = length $ nub $ fld ps i

display :: [Point] -> String
display ps = unlines $ map (map f) aps
  where m = M.fromList $ map (, 1) ps
        xmax = maximum $ map (\(Point x _) -> x) ps
        ymax = maximum $ map (\(Point _ y) -> y) ps
        aps = [[Point x y | x <- [0..xmax]] | y <- [0..ymax]]
        f :: Point -> Char
        f p
          | M.member p m = '#'
          | otherwise = ' '


part2 :: [Point] -> [Inst] -> String
part2 ps is = display fs
  where fs = foldl fld ps is


solve :: [String] -> String
solve ls = unlines [show p1, p2]
  where lines = splitLines ls ""
        coords = parseCoords $ head lines
        insts = parseInsts $ last lines
        p1 = part1 coords (head insts)
        p2 = part2 coords insts

main :: IO ()
main = interact $ solve . lines
