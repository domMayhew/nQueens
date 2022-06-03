import Data.List
import Data.Ord
import Codec.Picture
import Data.List.NonEmpty (NonEmpty ((:|)))
import System.Environment (getArgs)

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
data Board = Board {size :: Int, queens :: [Coord], safes :: [Coord]}

-- Makes an empty board
start :: Int -> Board
start n = Board n [] [(x,y) | x <- range, y <- range]
    where range = [0..(n-1)]

-- Returns true if the coordinate is not occupied and not seen by any Queens.
-- A linear search through an ordered list. There's probably a faster search in base/Prelude somewhere.
isSafe :: Coord -> Board -> Bool
isSafe coord (Board _ _ []) = False
isSafe coord (Board s queens (safe:safes))
  | safe > coord = False
  | safe == coord = True
  | otherwise = isSafe coord $ Board s queens safes

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
placeQueen coord (Board s queens safes)
  | isSafe coord (Board s queens safes) = Just $ Board s (coord:queens) (filter (not . sees coord) safes)
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

translate :: Int -> Int -> Picture -> Picture
translate x y (Picture pic w h) = Picture (\(x', y') -> pic (x'-x, y'-y)) w h


-- Creates a picture of a board with borders between each cell, around the entire board,
-- and padding around the outside border.
boardPic :: Template -> Board -> Picture
boardPic d b = translate (padding d - border d) (padding d - border d) (Picture f picSize picSize)
  where f (x,y)
          -- Horizontal padding
          | x < 0 || x >= picSizeNoPadding = white
          -- Vertical padding
          | y < 0 || y >= picSizeNoPadding = white
          -- Horizontal dividing borders
          | (x `mod` (squareSize d + border d)) < border d = black
          -- Vertical dividing borders
          | (y `mod` (squareSize d + border d)) < border d = black
          -- Queens.
          | (x `div` (border d + squareSize d),y `div` (border d + squareSize d)) `elem` queens b = red
          -- Empty spaces
          | otherwise = white
        picSize = size b * (squareSize d + border d) - border d + 2 * padding d
        picSizeNoPadding = size b * (border d + squareSize d) + border d

-- Creates a picture of a board with an external border and padding,
-- but no borders between cells.
-- The border is 1px. The only customizable dimension is the size of each square.
boardPicSimple :: Int -> Board -> Picture
boardPicSimple squareSize b = Picture pic picSize picSize
  where pic (x,y)
          -- Horizontal padding
          | x < squareSize - 1 || x >= squareSize + size b * squareSize + 1 = white
          -- Vertical padding
          | y < squareSize - 1 || y >= squareSize + size b * squareSize + 1 = white
          -- Left and right borders
          | x < squareSize || x == squareSize * (size b + 1) = black
          -- Top and bottom borders
          | y < squareSize || y == squareSize * (size b + 1) = black
          -- Queens
          | (div x squareSize - 1,div y squareSize - 1) `elem` queens b = red
          -- Empty spaces
          | otherwise = white
        picSize = (size b + 2) * squareSize


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
main = do
  args <- getArgs
  renderResults
    (if length args > 1 && args !! 1 == "simple" then boardPicSimple 2 else boardPic dimensions)
    (read $ head args)
    ("./images/results" ++ head args ++ ".png")