import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)
import Debug.Trace (trace)

firstDigit :: String -> Char
firstDigit [] = '0'
firstDigit (x : xs)
    | isDigit x = x
    | otherwise = firstDigit xs

lastDigit :: String -> Char
lastDigit [] = '0'
lastDigit xs
    | isDigit x = x
    | otherwise = lastDigit $ init xs
  where
    x = last xs

parseNum :: String -> String
parseNum xs = firstDigit xs : lastDigit xs : []

part1 :: [String] -> Int
part1 = sum . map (read . parseNum)

one = "one"
two = "two"
three = "three"
four = "four"
five = "five"
six = "six"
seven = "seven"
eight = "eight"
nine = "nine"

firstDigit2 :: String -> Int
firstDigit2 [] = 0
firstDigit2 xx@(x : xs)
    | isPrefixOf one xx = 1
    | isPrefixOf two xx = 2
    | isPrefixOf three xx = 3
    | isPrefixOf four xx = 4
    | isPrefixOf five xx = 5
    | isPrefixOf six xx = 6
    | isPrefixOf seven xx = 7
    | isPrefixOf eight xx = 8
    | isPrefixOf nine xx = 9
    | isDigit x = digitToInt x
    | otherwise = firstDigit2 xs

lastDigit2 :: String -> Int
lastDigit2 [] = 0
lastDigit2 xs
    | isPrefixOf (reverse one) rs = 1
    | isPrefixOf (reverse two) rs = 2
    | isPrefixOf (reverse three) rs = 3
    | isPrefixOf (reverse four) rs = 4
    | isPrefixOf (reverse five) rs = 5
    | isPrefixOf (reverse six) rs = 6
    | isPrefixOf (reverse seven) rs = 7
    | isPrefixOf (reverse eight) rs = 8
    | isPrefixOf (reverse nine) rs = 9
    | isDigit x = digitToInt x
    | otherwise = lastDigit2 $ init xs
  where
    rs = reverse xs
    x = last xs

parseNum2 :: String -> Int
parseNum2 xs = fn * 10 + ln
  where
    fn = firstDigit2 xs
    ln = lastDigit2 xs

part2 :: [String] -> Int
part2 = sum . map parseNum2

-- main :: IO ()
main = interact $ unlines . map show . sequence [part1, part2] . lines
