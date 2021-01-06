tree = '#'
open = '.'

data Pos = Pos Int Int
data Slope = Slope Int Int

type Terrain = [String]

atPos :: Pos -> Terrain -> Char
atPos (Pos x y) ts = chr
  where row = ts !! y
        chr = (!! x) $ concat $ repeat row

movePos :: Pos -> Slope -> Pos
movePos (Pos x y) (Slope a b) = Pos (x + a) (y + b)

isTree :: Pos -> Terrain -> Bool
isTree = ((== tree) .) . atPos

isOpen :: Pos -> Terrain -> Bool
isOpen = (.) (== open) . atPos

treeCount :: Pos -> Slope -> Terrain -> Int
treeCount p@(Pos _ y) s ts
  | y >= length ts = 0
  | isTree p ts = 1 + treeCount np s ts
  | otherwise = treeCount np s ts
  where np = movePos p s

part1 :: Slope -> Terrain -> Int
part1 = treeCount (Pos 0 0)

part2 :: [Slope] -> Terrain -> Int
part2 ss ts = foldl (*) 1 $ map (\s -> part1 s ts) ss

solve :: Terrain -> String
solve ts = unlines $ map show $ [ part1 (Slope 3 1) ts
                                , part2 [ Slope 1 1
                                        , Slope 3 1
                                        , Slope 5 1
                                        , Slope 7 1
                                        , Slope 1 2
                                        ] ts
                                ]

main :: IO ()
main = interact $ solve . lines
