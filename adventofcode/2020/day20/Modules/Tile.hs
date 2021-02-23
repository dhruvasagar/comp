module Modules.Tile ( Tile
                    , tileID
                    , tiles
                    , adjacencyMatrix
                    ) where

import Data.List
import qualified Data.Text as T

data Tile = Tile { tileID :: Int
                 , pixels :: [String]
                 }
                 deriving (Eq, Show)


parseTileID :: String -> Int
parseTileID = read . init . last . words

parseTile :: [String] -> Tile
parseTile ts = Tile { tileID = tileID
                    , pixels = pixels
                    }
  where tileID = parseTileID $ head ts
        pixels = tail ts

tiles :: String -> [Tile]
tiles = map (parseTile . lines . T.unpack) . T.splitOn tileDelim . T.pack
  where tileDelim = T.pack "\n\n"

topEdge :: Tile -> String
topEdge = head . pixels

bottomEdge :: Tile -> String
bottomEdge = last . pixels

leftEdge :: Tile -> String
leftEdge = map head . pixels

rightEdge :: Tile -> String
rightEdge = map last . pixels

edges :: Tile -> [String]
edges tile = map ($ tile) [ topEdge
                          , rightEdge
                          , bottomEdge
                          , leftEdge
                          ]

isNeighbor :: Tile -> Tile -> Bool
isNeighbor t1 t2 = not $ null es
  where es = [(e1, e2) |
               e1 <- edges t1
             , e2 <- edges t2
             , e1 == e2 || e1 == reverse e2
             ]

adjacencyMatrix :: [Tile] -> [(Tile, Int)]
adjacencyMatrix tiles = map f tiles
  where ots tile = tiles \\ [tile]
        ncnt tile = length $ filter (isNeighbor tile) $ ots tile
        f tile = (tile, ncnt tile)
