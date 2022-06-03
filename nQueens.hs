import Data.List
import Data.Ord
import Codec.Picture

----------------------------------------------------------------
-- Finding the Solutions
----------------------------------------------------------------

-- Data types
-- A board is a tuple (size, queens, safe)
-- where size is the height/width of the board
-- queens is a list of the coordinates of Queens
-- and safe is a list of unoccupied squares
-- that cannot be seen by any Queen.
type Coord = (Int, Int)
type Board = (Int, [Coord], [Coord])

-- Makes an empty board
start :: Int -> Board
start n = (n, [], [(x,y) | x <- range, y <- range])
    where range = [0..(n-1)]

-- Returns the size of the board
size :: Board -> Int
size (s, _, _) = s

-- Gets the list of spaces that can receive a queen
safes :: Board -> [Coord]
safes (_, _, safeSquares) = safeSquares

-- Gets the list of coordinates of queens on the board
queens :: Board -> [Coord]
queens (_, queens, _) = queens

-- Returns true if the coordinate is not occupied and not seen by any Queens.
-- A linear search through an ordered list. There's probably a faster search in base/Prelude somewhere.
isSafe :: Coord -> Board -> Bool
isSafe coord (s, queens, []) = False
isSafe coord (s, queens, safe:safes)
  | safe > coord = False
  | safe == coord = True
  | otherwise = isSafe coord (s, queens, safes)

-- Returns true if a queen can move from one coordinate to the other in one move
sees :: Coord -> Coord -> Bool
sees (r, c) (r', c')
  | r' == r = True
  | c' == c = True
  | (r' - r) == (c' - c) = True
  | (r' - r) == (c - c') = True
  | otherwise = False

-- Returns the board that results from placing a queen coord
-- or Nothing if the coord is not available.
placeQueen :: Coord -> Board -> Maybe Board
placeQueen coord board
  | isSafe coord board = Just (size board, coord:queens board, filter (not . sees coord) (safes board))
  | otherwise = Nothing

-- From a starting position, returns all the possible finished boards
solve :: Maybe Board -> [Board]
solve Nothing = []
solve (Just b)
  | length (queens b) == size b = [b]
  | null $ safes b        = []
  | otherwise = foldl join [] nextRow -- Concatenates solutions from all possible placements in the next row.
      where join = \boards coord -> boards ++ solve (placeQueen coord b)
            nextRow = takeWhile (\(r,c) -> r == length (queens b)) (safes b)

-- Run the thing!
results :: Int -> [Board]
results size = solve (Just $ start size)

---------------------------------------------------------
-- String Conversion

-- Get a string
prettify :: Board -> String
prettify b = go (0,0)
    where go :: Coord -> String
          go (r,c)
            | c == size b = '\n':go (nextRow (r,c))
            | r == size b = []
            | otherwise = currChar (r,c):go (nextCol (r,c))
          currChar :: Coord -> Char
          currChar coord
            | coord `elem` queens b = 'Q'
            | isSafe coord b = '.'
            | otherwise = '#'

-- Print a string
printBoard :: Board -> IO ()
printBoard = putStrLn . prettify

-- Gets the first square in the next row.
-- E.g., (3,2) -> (4,0)
nextRow :: Coord -> Coord
nextRow (r,c) = (r+1,0)

-- Gets the next square in the same row.
-- E.g., (3,2) -> (3,3)
nextCol :: Coord -> Coord
nextCol (r,c) = (r,c+1)

---------------------------------------------------------
-- Convert a board to a picture

-- Basic data types for creating images. Uses JuicyPixel's Code.Picture module

data Color = Color Int Int Int
  deriving (Show, Eq)

black = Color 0 0 0
white = Color 255 255 255
red = Color 255 0 0

type Picture = Coord -> Color

render :: Picture -> Int -> Int -> String -> IO ()
render picture w h name = writePng name (generateImage (\x y -> colorToPixel (picture (x,y))) w h)
  where colorToPixel :: Color -> PixelRGB8
        colorToPixel (Color r g b) = PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)

data Dimensions = Dimensions {border :: Int, squareSize :: Int, padding :: Int}
dimensions = Dimensions 2 20 20

totalWidth :: Int -> Dimensions -> Int
totalWidth squares d = widthNoPadding squares d + padding d * 2

widthNoPadding :: Int -> Dimensions -> Int
widthNoPadding squares d = squares * (border d + squareSize d) + border d

boardPic :: Dimensions -> Board -> Picture
boardPic d b = translate (padding d) (padding d) pic
  where pic (x,y)
          -- Left and top padding
          | x < 0 || y < 0 = white
          -- Right overflow
          | x >= picSize = white
          -- Bottom overflow
          | y >= picSize = white
          -- Left and top borders
          | x < border d || y < border d = black
          -- Horizontal dividing borders
          | (x `mod` (squareSize d + border d)) < border d = black
          -- Vertical dividing borders
          | (y `mod` (squareSize d + border d)) < border d = black
          -- Queens
          | (x `div` (border d + squareSize d),y `div` (border d + squareSize d)) `elem` queens b = red
          | otherwise = white
        picSize = widthNoPadding (size b) d

boardPicSimple :: Int -> Board -> Picture
boardPicSimple squareSize b = pic
  where pic (x,y)
          | x == squareSize - 1 || y == squareSize -1 = black
          | x == squareSize * (size b + 1) || y == squareSize * (size b + 1) = black
          | (div x squareSize - 1,div y squareSize - 1) `elem` queens b = red
          | otherwise = white

zoom :: Int -> Dimensions -> Dimensions
zoom n d = Dimensions (border d * n) (squareSize d * n) (padding d * n)

combineColor (Color 0 0 0) c = c
combineColor c (Color 0 0 0) = c
combineColor (Color 255 255 255) c = c
combineColor c (Color 255 255 255) = c
combineColor (Color r g b) (Color r' g' b') = Color (r+r' `div` 2) (g+g' `div` 2) (b+b' `div` 2)

combine :: Picture -> Picture -> Picture
combine p p' coord = combineColor (p coord) (p' coord)

translate :: Int -> Int -> Picture -> Picture
translate x y p (x', y') = p (x'-x,y'-y)

getBoardPics :: Dimensions -> [Board] -> Picture
getBoardPics d bs = go 0 $ map (boardPic d) bs
  where go :: Int -> [Picture] -> Picture
        go _ [] = const black
        go count (p:ps) = translate 0 (totalWidth (size $ head bs) d * count) p `combine` go (count+1) ps

grid :: Int -> Int -> [Picture] -> Picture
grid picSize rowSize ps = pic
  where pic (x,y)
          | index < length ps = (ps !! index) (x `mod` picSize,y `mod` picSize)
          | otherwise = white
          where index = (y `div` picSize) * rowSize + (x `div` picSize)

renderResults :: Int -> Int -> String -> IO ()
renderResults boardSize z =
  render pic width height
    where pic = grid picSize rowSize $ map (boardPic d) results
          results = results boardSize
          d = zoom z dimensions
          picSize = totalWidth boardSize d
          rowSize = unaryRoundUp sqrt $ length results
          width = picSize * rowSize
          height = binaryRoundUp (/) (length results) rowSize * picSize

renderSimpleResults :: Int -> String -> IO ()
renderSimpleResults boardSize =
  render pic width height
    where pic = grid picSize rowSize $ map (boardPicSimple squareSize) results
          results = results boardSize
          picSize = (boardSize + 2) * squareSize
          rowSize = unaryRoundUp sqrt $ length results
          width = picSize * rowSize
          height = binaryRoundUp (/) (length results) rowSize * picSize
          squareSize = 2


binaryRoundUp :: RealFrac a => (a -> a-> a) -> Int -> Int -> Int
binaryRoundUp op x y = ceiling $ op (fromIntegral x) (fromIntegral y)

unaryRoundUp :: RealFrac a => (a -> a) -> Int -> Int
unaryRoundUp op x = ceiling . op $ fromIntegral x

main :: IO ()
main = print . length $ results 15