module Board
  ( showBoard,
    readBoard,
    readPiece,
    emptyBoard,
    initialBoard,
    emptySquares,
    opponentColor,
    getColOf,
    getRowOf,
    Piece (..),
    Board,
    Square,
  )
where

type Row = Int

type Column = Int

type Square = Maybe Piece

type Board = [[Square]]

data Piece = White | Black deriving (Eq, Show)

showPiece :: Piece -> Char
showPiece White = 'w'
showPiece Black = 'b'

showSquare :: Square -> Char
showSquare Nothing = '_'
showSquare (Just p) = showPiece p

readPiece :: Char -> Piece
readPiece 'w' = White
readPiece 'b' = Black
readPiece _ = error "Piece not recognized!"

-- Does not read a String, reads a Char!!
readSquare :: Char -> Square
readSquare '_' = Nothing
readSquare p = Just (readPiece p)

-- have to remember that the first character is who plays, not part of board!
-- but we can fix that later :)
readBoard :: String -> Board
readBoard = map readRow . lines
  where
    readRow = map readSquare

showBoard :: Board -> String
showBoard = unlines . map showRow
  where
    showRow = map showSquare

emptyBoard :: Board
emptyBoard = replicate 8 row
  where
    row = replicate 8 Nothing

initialBoard :: Board
initialBoard = addPiece Black (4, 4) (addPiece White (4, 3) (addPiece White (3, 4) (addPiece Black (3, 3) emptyBoard)))

opponentColor :: Piece -> Piece
opponentColor Black = White
opponentColor White = Black

addPiece :: Piece -> (Int, Int) -> Board -> Board
addPiece piece (x, y) board = upperRows ++ [updateRow row piece x] ++ lowerRows
  where
    (upperRows, row : lowerRows) = splitAt y board

-- split at col, remove first elem in right list, join back together
updateRow :: [Square] -> Piece -> Column -> [Square]
updateRow r p c = xs ++ [Just p] ++ ys
  where
    (xs, _ : ys) = splitAt c r

getRowOf :: Board -> (Int, Int) -> Maybe [Square]
getRowOf board (_, y)
  | y >= 0 && length board > y = Just (board !! y)
  | otherwise = Nothing

getColOf :: Board -> (Int, Int) -> Maybe [Square]
getColOf board (x, _)
  | x >= 0 && length (head board) > x = Just (map (!! x) board)
  | otherwise = Nothing

-- TODO: Implement this! Check if there is some nice transpose function we could use
-- in order to make diagonals rows or columns, and then just apply one of the other functions on that!
getDiagonalsOf :: Board -> (Int, Int) -> Maybe [Square]
getDiagonalsOf = undefined

-- should return the coordinates of playable squares
-- (0,0) is in the top left
-- TODO: Make sure this works properly
emptySquares :: Board -> [(Int, Int)]
emptySquares [] = []
emptySquares rows = concat $ zipWith zipRowNumber [0 ..] (map emptyInRow rows)

zipRowNumber :: Int -> [Int] -> [(Int, Int)]
zipRowNumber _ [] = []
zipRowNumber n xs = zip (repeat n) xs

-- zip :: [a] -> [b] -> [(a,b)]

emptyInRow :: [Square] -> [Int]
emptyInRow [] = []
emptyInRow sqs = emptyInRow' sqs 0

emptyInRow' :: [Square] -> Int -> [Int]
emptyInRow' [] _ = []
emptyInRow' (Nothing : sqs) index = index : emptyInRow' sqs (index + 1)
emptyInRow' (_ : sqs) index = emptyInRow' sqs (index + 1)
