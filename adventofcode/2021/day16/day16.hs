import Data.List
import Debug.Trace (trace)

data Packet = Literal Int Int | Operator Int Int [Packet]
  deriving (Eq, Show)

hex2bits :: Char -> String
hex2bits '0' = "0000"
hex2bits '1' = "0001"
hex2bits '2' = "0010"
hex2bits '3' = "0011"
hex2bits '4' = "0100"
hex2bits '5' = "0101"
hex2bits '6' = "0110"
hex2bits '7' = "0111"
hex2bits '8' = "1000"
hex2bits '9' = "1001"
hex2bits 'A' = "1010"
hex2bits 'B' = "1011"
hex2bits 'C' = "1100"
hex2bits 'D' = "1101"
hex2bits 'E' = "1110"
hex2bits 'F' = "1111"

hex2bin :: String -> String
hex2bin = intercalate "" . map hex2bits

bin2dec :: String -> Int
bin2dec = foldl (\acc i -> acc * 2 + f i) 0
  where f :: Char -> Int
        f '0' = 0
        f '1' = 1
        f _ = 0

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
  where (as,bs) = splitAt n xs

parseLiteral :: String -> (String, Int)
parseLiteral "" = ("", 0)
parseLiteral s = (rs, n)
  where fs = splitEvery 5 s
        ob = takeWhile ((== '1') . head) fs
        fb = take 1 $ drop (length ob) fs
        n = bin2dec $ intercalate "" $ map tail $ ob ++ fb
        rs = intercalate "" $ drop (1 + length ob) fs

parseOperatorByLength :: String -> (String, [Packet])
parseOperatorByLength bs = (rs, parsePackets ls)
  where len = bin2dec $ take 15 bs
        bbs = drop 15 bs
        ls = take len bbs
        rs = drop len bbs

parseOperatorByCount :: String -> (String, [Packet])
parseOperatorByCount bs = parseNPackets count bbs
  where count = bin2dec $ take 11 bs
        bbs = drop 11 bs

parseOperator :: String -> (String, [Packet])
parseOperator "" = ("", [])
parseOperator (b:bs)
  | b == '0' = parseOperatorByLength bs
  | otherwise = parseOperatorByCount bs

parsePacket :: String -> (String, Packet)
parsePacket "" = ("", Literal 0 0)
parsePacket (v1:v2:v3 : t1:t2:t3: bs)
  | type_id == 4 = (ls, Literal version val)
  | otherwise = (os, Operator version type_id ps)
  where version = bin2dec [v1,v2,v3]
        type_id = bin2dec [t1,t2,t3]
        (ls, val) = parseLiteral bs
        (os, ps) = parseOperator bs

parsePackets :: String -> [Packet]
parsePackets ss
  | rs == "" = [p]
  | otherwise = p : parsePackets rs
  where (rs, p) = parsePacket ss

parseNPackets :: Int -> String -> (String, [Packet])
parseNPackets 0 ss = (ss, [])
parseNPackets n ss = (rss, p:ps)
    where (rs, p) = parsePacket ss
          (rss, ps) = parseNPackets (n - 1) rs

dropNPackets :: Int -> String -> String
dropNPackets 0 ss = ss
dropNPackets n ss = dropNPackets (n - 1) rs
  where (rs, _) = parsePacket ss

versionSum :: Packet -> Int
versionSum (Literal version _) = version
versionSum (Operator version _ packets) = (+ version) $ sum $ map versionSum packets

value :: Packet -> Int
value (Literal _ v) = v
value (Operator _ tid packets)
  | tid == 0 = sum $ map value packets
  | tid == 1 = product $ map value packets
  | tid == 2 = minimum $ map value packets
  | tid == 3 = maximum $ map value packets
  | tid == 5 = if p1 > p2 then 1 else 0
  | tid == 6 = if p1 < p2 then 1 else 0
  | tid == 7 = if p1 == p2 then 1 else 0
  | otherwise = 0
    where [p1, p2] = map value $ take 2 packets

part1 :: String -> Int
part1 ss = versionSum p
  where (_, p) = parsePacket $ hex2bin ss

part2 :: String -> Int
part2 ss = value p
  where (_, p) = parsePacket $ hex2bin ss

solve :: String -> String
solve s = intercalate "\n" $ map (show . ($ s)) [part1, part2]

main :: IO ()
main = interact $ solve . head . words
