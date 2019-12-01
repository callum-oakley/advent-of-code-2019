fuel :: Integer -> Integer
fuel mass = mass `div` 3 - 2

parse :: String -> [Integer]
parse = map read . lines

main = do
  masses <- parse <$> getContents
  putStr "part 1: "
  print . sum . map fuel $ masses
