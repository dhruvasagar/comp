import qualified Data.Map as M
import Data.Maybe (fromJust)
import Debug.Trace (trace)

type Name = String

data Operator
    = Plus
    | Minus
    | Mul
    | Div
    deriving (Show)

parseOperator :: String -> Operator
parseOperator "+" = Plus
parseOperator "-" = Minus
parseOperator "*" = Mul
parseOperator "/" = Div

data Job
    = YellNumber Int
    | YellMathOperation Name Operator Name
    deriving (Show)

type Jobs = M.Map Name Job

parseJob :: Jobs -> String -> Jobs
parseJob jobs js
    | size == 2 = M.insert key yn jobs
    | otherwise = M.insert key ym jobs
  where
    ws = words js
    size = length ws
    key = init $ head ws
    yn = YellNumber $ read $ last ws
    val = tail ws
    ym = YellMathOperation (head val) (parseOperator $ head $ drop 1 val) (last val)

parseJobs :: [String] -> Jobs
parseJobs js = foldl parseJob M.empty js

operate :: Operator -> Int -> Int -> Int
operate Plus a b = a + b
operate Minus a b = a - b
operate Mul a b = a * b
operate Div a b = a `div` b

calc :: Name -> Int -> Jobs -> Int
calc name humn jobs
    | name == "humn" && humn >= 0 = humn
    | otherwise = fromJust $ f <$> job
  where
    job = M.lookup name jobs
    f :: Job -> Int
    f (YellNumber n) = n
    f (YellMathOperation n1 op n2) = operate op (calc n1 humn jobs) (calc n2 humn jobs)

part1 :: Jobs -> Int
part1 = calc "root" (-1)

binSearch :: Int -> Int -> Name -> Name -> Jobs -> Int
binSearch l h n1 n2 jobs
    | diff == 0 = m
    | diff > 0 = binSearch m h n1 n2 jobs
    | otherwise = binSearch l m n1 n2 jobs
  where
    m = (l + h) `div` 2
    diff = (calc n1 m jobs) - (calc n2 m jobs)

part2 :: Jobs -> Int
part2 jobs = binSearch 0 (10 ^ 15) n1 n2 jobs
  where
    (YellMathOperation n1 _ n2) = fromJust $ M.lookup "root" jobs

main :: IO ()
main = interact $ unlines . map show . sequence [part1, part2] . parseJobs . lines
