import Modules.Tile

solve :: [Tile] -> Int
solve = product . map (tileID . fst) . filter ((== 2) . snd) . adjacencyMatrix

main :: IO ()
main = interact $ show . solve . tiles
