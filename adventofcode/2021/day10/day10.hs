import Data.List (sort)
import Debug.Trace (trace)

pair :: Char -> Char
pair '}' = '{'
pair ']' = '['
pair ')' = '('
pair '>' = '<'
pair _ = '_'

revPair :: Char -> Char
revPair '{' = '}'
revPair '[' = ']'
revPair '(' = ')'
revPair '<' = '>'
revPair _ = '_'

syntaxScore :: Char -> Int
syntaxScore ')' = 3
syntaxScore ']' = 57
syntaxScore '}' = 1197
syntaxScore '>' = 25137
syntaxScore _ = 0

errorScore :: Char -> Int
errorScore ')' = 1
errorScore ']' = 2
errorScore '}' = 3
errorScore '>' = 4
errorScore _ = 0

isCorrupt :: String -> String -> Bool
isCorrupt [] _ = False
isCorrupt (c:cs) []
  | c == '[' || c == '(' || c == '{' || c == '<' = isCorrupt cs [c]
  | otherwise = True
isCorrupt (c:cs) s@(sf:ss)
  | c == '[' || c == '(' || c == '{' || c == '<' = isCorrupt cs (c:s)
  | otherwise = (sf /= pair c) || isCorrupt cs ss

corruptChar :: String -> String -> Char
corruptChar [] (s:ss) = revPair s
corruptChar (c:cs) []
  | c == '[' || c == '(' || c == '{' || c == '<' = corruptChar cs [c]
  | otherwise = c
corruptChar (c:cs) s@(sf:ss)
  | c == '[' || c == '(' || c == '{' || c == '<' = corruptChar cs (c:s)
  | otherwise = if sf /= pair c then c else corruptChar cs ss

complete :: String -> String -> String
complete [] s = map revPair s
complete (c:cs) []
  | c == '[' || c == '(' || c == '{' || c == '<' = complete cs [c]
  | otherwise = []
complete (c:cs) s@(sf:ss)
  | c == '[' || c == '(' || c == '{' || c == '<' = complete cs (c:s)
  | otherwise = if sf /= pair c then map revPair s else complete cs ss

part1 :: [String] -> Int
part1 ss = sum $ map (syntaxScore . (`corruptChar` "")) corrupt
  where corrupt = filter (`isCorrupt` "") ss

mid :: [Int] -> Int
mid xs = xs !! idx
  where idx = div (length xs) 2

part2 :: [String] -> Int
part2 ss = mid $ sort $ map (foldl (\a b -> a * 5 + b) 0 . map errorScore) completion
  where incomplete = filter (not . (`isCorrupt` "")) ss
        completion = map (`complete` "") incomplete

mapm :: [a -> b] -> a -> [b]
mapm fs x = map ($ x) fs

main :: IO ()
main = interact $ unlines . map show . mapm [part1, part2] . words
