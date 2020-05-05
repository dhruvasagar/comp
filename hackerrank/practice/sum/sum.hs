main = interact $ show . sum . map read . tail . words
