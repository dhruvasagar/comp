type Coord = (Int, Int)

moveUp :: Coord -> Coord
moveUp (x, y) = (x, y + 1)

moveDown :: Coord -> Coord
moveDown (x, y) = (x, y - 1)

move :: Char -> Coord -> Coord
move 'U' = moveUp
move 'D' = moveDown

moves :: [Char] -> Coord -> Int
moves [] _ = 0
moves (c:cs) coord = val + moves cs nc
  where nc = move c coord
        val = if snd nc == 0 && c == 'U' then 1 else 0

solve :: String -> Int
solve ds = moves ds (0, 0)

main :: IO ()
main = interact $ show . solve . last . lines
