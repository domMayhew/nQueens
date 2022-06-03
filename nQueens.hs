import Data.List
import Data.Ord
import Codec.Picture
import Data.List.NonEmpty (NonEmpty ((:|)))

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

-- Picture f w h
data Picture = Picture (Coord -> Color) Int Int
apply :: Coord -> Picture -> Color
apply coord (Picture f _ _) = f coord

render :: Picture -> String -> IO ()
render (Picture f w h) name = writePng name (generateImage (\x y -> colorToPixel (f (x,y))) w h)
  where colorToPixel :: Color -> PixelRGB8
        colorToPixel (Color r g b) = PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)

-- Dimensions used by a picture factory function.
-- border: width of borders around each cell, or around whole image
-- squareSize: width & height of each square on the board
-- padding: white space surrounding the board
data Template = Template {border :: Int, squareSize :: Int, padding :: Int}

-- Default dimensions
dimensions :: Template
dimensions = Template 1 10 10

-- Helper function. Not currently used in this module.
zoom :: Int -> Template -> Template
zoom n d = Template (border d * n) (squareSize d * n) (padding d * n)

-- Creates a picture of a board with borders between each cell, around the entire board,
-- and padding around the outside border.
boardPic :: Template -> Board -> Picture
boardPic d b = Picture f picSize picSize
  where f (x,y)
          -- Left padding and border
          | x < padding d = topLeftPadding x
          -- Top padding and border
          | y < padding d = topLeftPadding y
          -- Horizontal dividing borders
          | (x `mod` (squareSize d + border d)) < border d = black
          -- Vertical dividing borders
          | (y `mod` (squareSize d + border d)) < border d = black
          -- Queens. Be aware that if the image being rendered is larger than the size of the board,
            -- It's better to have checks for x > picSize, y > picSize so that `queens b` isn't called
            -- unnecessarily. In this module, however, each picture should never receive coordinates >= picSize
          | (x `div` (border d + squareSize d),y `div` (border d + squareSize d)) `elem` queens b = red
          -- It's not a queen or border.
          | otherwise = white
          -- topLeftPadding only works if n < padding
          where topLeftPadding n = if n >= padding d - border d then black else white
        picSize = size b * (border d + squareSize d) + border d + padding d * 2

-- Creates a picture of a board with an external border and padding,
-- but no borders between cells.
-- The border is 1px. The only customizable dimension is the size of each square.
boardPicSimple :: Int -> Board -> Picture
boardPicSimple squareSize b = Picture pic picSize picSize
  where pic (x,y)
          | x == squareSize - 1 || y == squareSize -1 = black
          | x == squareSize * (size b + 1) || y == squareSize * (size b + 1) = black
          | (div x squareSize - 1,div y squareSize - 1) `elem` queens b = red
          | otherwise = white
        picSize = (size b + 1) * squareSize + 2


-- Combines a list of boards into a grid using the given picture factory function.
grid :: [Picture] -> Int -> Picture
grid [] _       = Picture (const white) 0 0
grid ps rowSize = Picture gridPic gridW gridH
  where Picture boardPic boardW boardH = head ps
        gridW = boardW * rowSize
        gridH = boardH * binaryRoundUp (/) (length ps) rowSize
        gridPic (x,y)
          | index < length ps = apply (x `mod` boardW,y `mod` boardH) (ps !! index)
          | otherwise = white
          where index = (y `div` boardH) * rowSize + (x `div` boardW)

-- Used to get height of a grid.
binaryRoundUp :: RealFrac a => (a -> a-> a) -> Int -> Int -> Int
binaryRoundUp op x y = ceiling $ op (fromIntegral x) (fromIntegral y)

-- Produces a PNG of solutions for a nSquares x nSquares board.
renderResults :: (Board -> Picture) -> Int -> String -> IO ()
renderResults f nSquares =
  render $ grid pics rowSize
  where solutions = results nSquares
        pics = map f solutions
        rowSize = unaryRoundUp sqrt $ length solutions

-- Used to get rowSize for a grid.
unaryRoundUp :: RealFrac a => (a -> a) -> Int -> Int
unaryRoundUp op x = ceiling . op $ fromIntegral x

-- Render results for a board size.
main :: IO ()
main = print . length $ results 12