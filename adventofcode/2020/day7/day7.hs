import Data.Char
import qualified Data.Map as M
import qualified Data.Text as T

data Bag = Bag Int T.Text deriving (Show)

instance Eq Bag where
  (==) (Bag x xs) (Bag y ys) = x == y && xs == ys

instance Ord Bag where
  compare (Bag x xs) (Bag y ys)
    | x == y = compare xs ys
    | otherwise = compare x y

comma = T.pack ","
bagDelim = T.pack "bag"
conDelim = T.pack " contain "

parseBag :: T.Text -> Bag
parseBag bs
  | isDigit c = Bag (digitToInt c) rs
  | otherwise = Bag 0 bs
  where
    c = T.head bs
    rs = T.strip $ T.tail bs

bagRule :: T.Text -> [(T.Text, [Bag])]
bagRule ls = [(pclr, cbags)]
  where
    [pbs, cbs] = map T.strip $ T.splitOn conDelim ls
    (Bag _ pclr) = parseBag $ head $ map T.strip $ T.splitOn bagDelim pbs
    bagf = parseBag . T.strip . head . T.splitOn bagDelim
    cbags = map bagf $ T.splitOn comma cbs

bagRules :: [T.Text] -> M.Map T.Text [Bag]
bagRules = M.fromList . concatMap bagRule

canContains :: M.Map T.Text [Bag] -> T.Text -> T.Text -> Bool
canContains bmap sclr bclr
  | M.notMember bclr bmap = False
  | any (\(Bag _ clr) -> sclr == clr) bags = True
  | otherwise = any (\(Bag _ clr) -> canContains bmap sclr clr) bags
  where (Just bags) = M.lookup bclr bmap

shinyBag = T.pack "shiny gold"
brightWhiteBag = T.pack "bright white"
darkOrangeBag = T.pack "dark orange"

part1 :: [T.Text] -> Int
part1 ls = length ks
  where bmap = bagRules ls
        ks = filter (canContains bmap shinyBag) $ M.keys bmap

contains :: M.Map T.Text [Bag] -> T.Text -> Int
contains bmap bclr
  | M.notMember bclr bmap = 0
  | otherwise = foldl f 0 bags
  where (Just bags) = M.lookup bclr bmap
        f r (Bag c clr) = r + (1 + contains bmap clr) * c

part2 :: [T.Text] -> Int
part2 ls = contains bmap shinyBag
  where bmap = bagRules ls

solve :: [T.Text] -> String
solve ls = unlines $ map show [part1 ls, part2 ls]

main :: IO ()
main = interact $ solve . T.lines . T.pack
