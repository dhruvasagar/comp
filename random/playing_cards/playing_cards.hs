module Main where

import Data.List
import System.Random
import Data.Array.IO
import Control.Monad

data Suit = Club
          | Diamond
          | Heart
          | Spade
          deriving (Read, Show, Enum, Eq, Ord)

data Rank = Ace
          | Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          deriving (Read, Show, Enum, Eq, Ord)

data Card = Card Suit Rank

instance Show Card where
  show card = (show rank) ++ " of " ++ (show suit)
    where (Card suit rank) = card

instance Eq Card where
  (==) c1 c2 = suit1 == suit2 && rank1 == rank2
    where (Card suit1 rank1) = c1
          (Card suit2 rank2) = c2

instance Ord Card where
  compare c1 c2 = compare (suit1, rank1) (suit2, rank2)
    where (Card suit2 rank2) = c2
          (Card suit1 rank1) = c1

type Deck = [Card]

-- Borrowed from random_shuffle
-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs = newListArray (1,n) xs

deckStr :: Deck -> String
deckStr = unlines . map show

makeDeck :: Deck
makeDeck = Card <$> [Club ..] <*> [Ace ..]
-- same as
-- makeDeck = map Card [Club ..] <*> [Ace ..]
-- or using list comprehensions
-- makeDeck = [Card suit rank | suit <- [Club ..], rank <- [Ace ..]]

peak_top :: Deck -> Card
peak_top = head

peak_bottom :: Deck -> Card
peak_bottom = last

deal :: Deck -> (Card, Deck)
deal (card:ndeck) = (card, ndeck)

dealHand :: Deck -> Int -> (Deck, Deck)
dealHand deck n = (fdeck, ndeck)
  where fdeck = take n deck
        ndeck = drop n deck

deckInsert :: Deck -> Int -> Card -> Deck
deckInsert deck index card = ndeck
  where cardsTillIndex = take (index - 1) deck
        cardsBeyondIndex = drop (index + 1) deck
        ndeck = cardsTillIndex ++ [card] ++ cardsBeyondIndex

-- Return back a hand of cards (also a deck) to a deck
deckMerge :: Deck -> Deck -> Deck
deckMerge = (++)

main :: IO ()
main = do
  deck <- shuffle makeDeck
  putStrLn $ "The top card is the " ++ (show $ peak_top deck)
  putStrLn $ "The bottom card is the " ++ (show $ peak_bottom deck)
  putStrLn $ deckStr $ sort deck
  let (card, ndeck) = deal deck
  putStrLn $ "Drawn card is " ++ (show card)
  let ndeck2 = deckInsert ndeck (length ndeck) card
  putStrLn $ "Inserted card at the end\n" ++ deckStr ndeck2
