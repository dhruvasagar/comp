{-# LANGUAGE RecordWildCards #-}

import Data.List

data Case = Case
  { low :: Int
  , high :: Int
  , char :: Char
  , pass :: String
  }

policy :: String -> Case
policy ss = Case{low=l, high=h, char=c, pass=pass}
  where
    (Just ci) = elemIndex ':' ss
    (policy, _:_:pass) = splitAt ci ss
    (Just di) = elemIndex '-' policy
    (ll, _:hl) = splitAt di policy
    (Just si) = elemIndex ' ' hl
    (hh, _:c:_) = splitAt si hl
    l = read ll :: Int
    h = read hh :: Int

charCount :: Char -> String -> Int
charCount _ [] = 0
charCount ch [c]
  | ch == c = 1
  | otherwise = 0
charCount ch (c:cs) = charCount ch [c] + charCount ch cs

valid1 :: Case -> Bool
valid1 Case{..} = low <= n && n <= high
  where n = charCount char pass

part1 :: [String] -> Int
part1 = length . filter (valid1 . policy)

valid2 :: Case -> Bool
valid2 Case{..}
  | pl == char && ph /= char = True
  | pl /= char && ph == char = True
  | otherwise = False
  where pl = pass !! (low - 1)
        ph = pass !! (high - 1)

part2 :: [String] -> Int
part2 = length . filter (valid2 . policy)

solve :: [String] -> String
solve ss = unlines $ map show [part1 ss, part2 ss]

main :: IO ()
main = interact $ solve . lines
