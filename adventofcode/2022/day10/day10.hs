import Debug.Trace (trace)

data Instruction = Noop | Addx Int
  deriving (Show)

cycleCount :: Instruction -> Int
cycleCount Noop = 1
cycleCount (Addx _) = 2

parseInstruction :: String -> Instruction
parseInstruction s
  | s == "noop" = Noop
  | otherwise = Addx v
  where v = read $ last $ words s

data Program = Program Int [Instruction]
  deriving (Show)

signalStrength :: Int -> Int -> Int
signalStrength = (*)

runInstruction :: Instruction -> Int -> Int -> Int
runInstruction Noop x _ = x
runInstruction (Addx _) x 0 = x
runInstruction (Addx v) x 1 = x + v

instructionCycleCount :: Instruction -> Int -> Int
instructionCycleCount Noop _ = 0
instructionCycleCount (Addx _) 0 = 1
instructionCycleCount (Addx _) 1 = 0

nextInstructions :: Instruction -> Int -> [Instruction] -> [Instruction]
nextInstructions Noop _ (i:is) = is
nextInstructions (Addx _) 0 is = is
nextInstructions (Addx _) 1 (i:is) = is

first (a, _, _) = a
second (_, a, _) = a
third (_, _, a) = a

run :: Program -> Int -> Int -> [(Int, Int, Int)] -> [(Int, Int, Int)]
run (Program x []) _ tcn ss = ss
run (Program x is) icn tcn ss = run (Program nx nis) nicn (tcn + 1) nss
  where i = head is
        nx = runInstruction i x icn
        nis = nextInstructions i icn is
        nicn = instructionCycleCount i icn
        nss = ss ++ [(tcn, x, signalStrength tcn x)]

indices = [20, 60, 100, 140, 180, 220]

part1 :: [Instruction] -> Int
part1 is = sum [third s | s <- output, elem (first s) indices]
  where program = (Program 1 is)
        output = run program 0 1 []

pixel :: Int -> Int -> Char
pixel p x
  | abs (p - x) < 2 = '#'
  | otherwise = ' '

display :: [Int] -> String
display [] = ""
display xs = line ++ "\n" ++ display nxs
  where line = zipWith pixel [0..39] $ take 40 xs
        nxs = drop 40 xs

part2 :: [Instruction] -> Int
part2 is = (trace (display $ map second output) 0)
  where program = (Program 1 is)
        output = run program 0 1 []

main :: IO ()
main = interact $ unlines . map show . sequence [part1, part2] . map parseInstruction . lines
