import Data.List

type Square = [[Int]]

rot :: Square -> Square
rot = map reverse . transpose

refl :: Square -> Square
refl = transpose

magic :: Square
magic = [ [8, 3, 4]
        , [1, 5, 9]
        , [6, 7, 2]
        ]

allMagic :: [Square]
allMagic = concat [ take 4 $ iterate rot magic
                  , take 4 $ iterate rot $ refl magic
                  ]

dist :: Square -> Square -> Int
dist s1 s2 = sum $ map abs $ zipWith (-) (concat s1) (concat s2)

solve :: Square -> Int
solve s = minimum $ map (dist s) allMagic

main :: IO ()
main = interact $ show . solve . map (map read . words) . lines
