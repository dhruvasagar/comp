import Data.Char

charHeight :: [Int] -> Char -> Int
charHeight hs c = hs !! idx
  where idx = ord c - ord 'a'

solve :: [String] -> Int
solve [ns, word] = mh * n
  where
    n = length word
    nums = map read $ words ns :: [Int]
    mh = maximum $ map (charHeight nums) word

main :: IO ()
main = interact $ show . solve . lines
