data Move = Rock | Paper | Scissor
  deriving (Show)

parseMove :: String -> Move
parseMove "A" = Rock
parseMove "X" = Rock
parseMove "B" = Paper
parseMove "Y" = Paper
parseMove "C" = Scissor
parseMove "Z" = Scissor

data Outcome = Lose | Draw | Win
  deriving (Show)

parseOutcome :: String -> Outcome
parseOutcome "X" = Lose
parseOutcome "Y" = Draw
parseOutcome "Z" = Win

moveScore :: Move -> Int
moveScore Rock = 1
moveScore Paper = 2
moveScore Scissor = 3

outcomeScore :: Outcome -> Int
outcomeScore Lose = 0
outcomeScore Draw = 3
outcomeScore Win = 6

outcome :: Move -> Move -> Outcome
outcome Rock Rock = Draw
outcome Rock Scissor = Lose
outcome Rock Paper = Win
outcome Paper Paper = Draw
outcome Paper Rock = Lose
outcome Paper Scissor = Win
outcome Scissor Scissor = Draw
outcome Scissor Paper = Lose
outcome Scissor Rock = Win

pickMove :: Move -> Outcome -> Move
pickMove Rock Draw = Rock
pickMove Rock Win = Paper
pickMove Rock Lose = Scissor
pickMove Paper Draw = Paper
pickMove Paper Win = Scissor
pickMove Paper Lose = Rock
pickMove Scissor Draw = Scissor
pickMove Scissor Win = Rock
pickMove Scissor Lose = Paper

roundScore :: Move -> Move -> Int
roundScore m1 m2 = moveScore m2 + outcomeScore out
  where out = outcome m1 m2

toTuple :: [a] -> (a, a)
toTuple [x, y] = (x, y)

toMoveOutcome :: (String, String) -> (Move, Move)
toMoveOutcome (x, y) = (m, pickMove m (parseOutcome y))
  where m = parseMove x

part1 :: [String] -> Int
part1 = sum . map (uncurry roundScore . toTuple . map parseMove . words)

part2 :: [String] -> Int
part2 = sum . map (uncurry roundScore . toMoveOutcome . toTuple . words)

main :: IO ()
main = interact $ unlines . map show . sequence [part1, part2] . lines
