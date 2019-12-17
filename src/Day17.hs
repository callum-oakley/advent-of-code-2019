module Day17 where

import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Linear.V2
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Intcode

type Direction = V2 Int

type Position = V2 Int

type Scaffolding = Set (V2 Int)

type Robot = (Position, Direction)

parseAscii :: String -> (Scaffolding, Robot)
parseAscii ascii =
  (Set.fromList $ catMaybes scaffolding, head $ catMaybes robots)
  where
    (scaffolding, robots) =
      unzip . concatMap parseLine . zip [0 ..] $ lines ascii
    parseLine (y, l) = map (\(x, c) -> parseChar (V2 x y, c)) $ zip [0 ..] l
    parseChar (pos, c) =
      case c of
        '^' -> (Just pos, Just (pos, V2 0 (-1)))
        'v' -> (Just pos, Just (pos, V2 0 1))
        '<' -> (Just pos, Just (pos, V2 (-1) 0))
        '>' -> (Just pos, Just (pos, V2 1 0))
        '#' -> (Just pos, Nothing)
        _   -> (Nothing, Nothing)

calibrate :: Scaffolding -> Int
calibrate s = sum . Set.map product $ Set.filter isIntersection s
  where
    isIntersection pos =
      all
        (\dir -> (pos + dir) `Set.member` s)
        [V2 0 (-1), V2 0 1, V2 (-1) 0, V2 1 0]

walk :: (Scaffolding, Robot) -> String
walk (s, (rpos, rdir)) =
  replicate forwardSteps 'F' <>
  case turn of
    Just t  -> t : walk (s, (rpos', rotate t rdir))
    Nothing -> []
  where
    forwardSteps =
      length $ takeWhile (\f -> (rpos + fmap (f *) rdir) `Set.member` s) [1 ..]
    rpos' = rpos + fmap (forwardSteps *) rdir
    turn = find (\t -> (rpos' + rotate t rdir) `Set.member` s) ['L', 'R']
    rotate 'L' (V2 x y) = V2 y (-x)
    rotate 'R' (V2 x y) = V2 (-y) x

shorten :: String -> String
shorten =
  intercalate "," .
  map
    (\g ->
       if head g == 'F'
         then show (length g)
         else g) .
  group

compressions :: Int -> [String] -> [[String]]
compressions 0 [] = [[]]
compressions _ [] = []
compressions 0 _ = []
compressions n ss@(s:_) =
  [ subsequence : c
  | m <- [2 .. length s]
  , let subsequence = take m s
  , length (shorten subsequence) <= 20
  , let ss' = concatMap (splitter subsequence) ss
  , c <- compressions (n - 1) ss'
  ]
  where
    splitter = split . dropBlanks . dropDelims . onSublist

compress :: String -> (String, String, String, String)
compress s = (compressWith compression, a, b, c)
  where
    compression =
      head . filter ((<= 20) . length . compressWith) $ compressions 3 [s]
    [a, b, c] = map shorten compression
    sub x y = intercalate y . splitOn x
    compressWith [aLong, bLong, cLong] =
      intersperse ',' . sub cLong "C" . sub bLong "B" . sub aLong "A" $ s

program :: IO Intcode.Program
program = Intcode.parse <$> readFile "data/input17"

part1 :: IO Int
part1 = do
  p <- program
  let ascii = map chr $ Intcode.run p []
  pure . calibrate . fst $ parseAscii ascii

part2Interactive :: IO ()
part2Interactive = do
  p <- program
  go . Intcode.runDynamic $ 2 : tail p
  where
    go (Intcode.Input f)     = getChar >>= go . f . ord
    go (Intcode.Output o c') = putStr (display o) >> go c'
    go Intcode.Stop          = pure ()
    display o
      | isAscii $ chr o = [chr o]
      | otherwise = show o

part2 :: IO Int
part2 = do
  p <- program
  let (main, a, b, c) =
        compress . walk . parseAscii . map chr $ Intcode.run p []
  pure . last . Intcode.run (2 : tail p) . map ord $
    unlines [main, a, b, c, "n"]

testInput :: String
testInput =
  unlines
    [ "..#.........."
    , "..#.........."
    , "#######...###"
    , "#.#...#...#.#"
    , "#############"
    , "..#...#...#.."
    , "..#####...^.."
    ]

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "day17"
    [ testCase "calibrate" $ (calibrate . fst $ parseAscii testInput) @?= 76
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= 3192
    , testCase "part2" $ do
        p2 <- part2
        p2 @?= 684691
    ]
