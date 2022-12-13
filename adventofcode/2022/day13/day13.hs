import Control.Applicative
import Data.Char (isSpace, isDigit, digitToInt)
import Data.List (isPrefixOf, sort, elemIndex)
import Data.Maybe (fromJust)
import Data.Ord
import Debug.Trace (trace)

split :: (a -> Bool) -> [a] -> [[a]]
split p xs = f xs []
  where f [] agg = [agg]
        f (y : ys) agg
          | p y = agg : f ys []
          | otherwise = f ys (agg ++ [y])

data Packet = Atom Int
            | List [Packet]
            deriving (Eq, Show)

instance Ord Packet where
  compare (Atom a) (Atom b) = compare a b
  compare (Atom a) b = compare (List [Atom a]) b
  compare a (Atom b) = compare a (List [Atom b])
  compare (List a) (List b)
    | all (== EQ) ac = compare (length a) (length b)
    | otherwise = head $ filter (/= EQ) ac
    where ac = zipWith compare a b

newtype Parser a = Parser
  { runParser ::String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP x = Parser f
  where f [] = Nothing
        f (c:cs)
          | c == x = Just (cs, x)
          | otherwise = Nothing

spanP :: (Char -> Bool) -> Parser String
spanP f =
  Parser $ \input ->
    let (token, rest) = span f input
     in Just (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    if null xs
      then Nothing
      else Just (input', xs)

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

parseAtom :: Parser Packet
parseAtom = f <$> notNull (spanP isDigit)
    where f ds = Atom $ read ds

parseList :: Parser Packet
parseList = List <$> (charP '[' *> ws *> elements <* ws <* charP ']')
  where elements = sepBy (ws *> charP ',' <* ws) parsePacket

parsePacket :: Parser Packet
parsePacket = parseAtom <|> parseList

parsePair :: [String] -> (Packet, Packet)
parsePair [p1, p2] = (snd $ fromJust $ (runParser parsePacket) p1, snd $ fromJust $ (runParser parsePacket) p2)

part1 :: [(Packet, Packet)] -> Int
part1 = sum . map fst . filter ((/= GT) . snd) .zip [1..] . map (uncurry compare)

unTuple :: (Packet, Packet) -> [Packet]
unTuple (p1, p2) = [p1, p2]

part2 :: [(Packet, Packet)] -> Int
part2 ps = product $ map ((+1) . fromJust) $ sequence [elemIndex ds1, elemIndex ds2] $ sort $ fs ++ [ds1, ds2]
  where ds1 = List [ List [ Atom 2] ]
        ds2 = List [ List [ Atom 6] ]
        fs = concat $ map unTuple ps

main :: IO ()
main = interact $ unlines . map show . sequence [part1, part2] . map parsePair . split (== "") . lines
