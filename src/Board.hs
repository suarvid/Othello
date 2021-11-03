module Board 
  (showBoard,
  readBoard,
  readPiece,
  Piece(..),
  Board,
  Square,
  ) where


type Square = Maybe Piece
type Board = [[Square]]
data Piece = White | Black deriving Eq

showPiece :: Piece -> Char
showPiece White = 'w'
showPiece Black = 'b'

showSquare :: Square -> Char
showSquare Nothing = ' '
showSquare (Just p) = showPiece p

readPiece :: Char -> Piece
readPiece 'w' = White
readPiece 'b' = Black
readPiece _ = error "Piece not recognized!"

-- Does not read a String, reads a Char!!
readSquare :: Char -> Square
readSquare ' ' = Nothing
readSquare p = Just(readPiece p)

-- have to remember that the first character is who plays, not part of board!
-- but we can fix that later :)
readBoard :: String -> Board
readBoard = map readRow . lines
  where readRow = map readSquare

showBoard :: Board -> String
showBoard = unlines . map showRow
  where showRow = map showSquare
