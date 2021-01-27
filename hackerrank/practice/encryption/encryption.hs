import Data.Char
import Data.List

squish :: String -> String
squish = foldr f []
  where f c s
          | isSpace c = s
          | otherwise = c:s

chop :: Int -> String -> [String]
chop _ [] = []
chop n ns = fs : chop n rs
  where fs = take n ns
        rs = drop n ns

chopSize :: Int -> Int
chopSize n
  | fn * fn >= n = fn
  | otherwise = cn
  where sn = sqrt $ fromIntegral n
        fn = fromIntegral $ floor sn
        cn = fromIntegral $ ceiling sn

encrypt :: String -> String
encrypt ns = unwords $ transpose cs
  where s = squish ns
        cn = chopSize $ length s
        cs = chop cn s

solve :: String -> String
solve = encrypt

main :: IO ()
main = interact $ solve . head . lines
