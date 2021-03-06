module Move
  ( makeMove,
    genValidMoves,
    genMoves,
    Move (..), -- export all value constructors
  )
where

import Board
import Data.Maybe
import Flip

-- A Move is just adding a piece to a position
data Move = Move Piece (Column, Row) deriving (Show)

type Column = Int

type Row = Int

-- TODO: Redundancy here, make use of addPiece in Board and delete updateRow
-- same pattern, here: split, update Row, rejoin
makeMove :: Board -> Move -> Board
makeMove b (Move piece (x, y)) = flipBoard (opponentColor piece) $ upperRows ++ [updateRow row piece x] ++ lowerRows
  where
    (upperRows, row : lowerRows) = splitAt y b

-- split at col, remove first elem in right list, join back together
updateRow :: [Square] -> Piece -> Column -> [Square]
updateRow r p c = xs ++ [Just p] ++ ys
  where
    (xs, _ : ys) = splitAt c r

-- Perhaps a bit weird that initialBoard is in Move
-- but setting up a Board does require making moves
-- Makes more sense than having makeMove in Board
-- and otherwise we get circular dependencies

-- parse a String of the form "w00" which means white at (0,0)
parseMove :: String -> Move
parseMove s =
  let piece = readPiece ps
      row = read rs
      col = read cs
   in Move piece (col, row)
  where
    ps = head s -- same as !! 0
    rs = [s !! 1]
    cs = [s !! 2]

genValidMoves :: Piece -> Board -> [Move]
genValidMoves color board = filter isValidMove allMoves
  where
    isValidMove = flip validMove board
    allMoves = genMoves color board

genMoves :: Piece -> Board -> [Move]
genMoves color board =
  map (Move color) freePositions
  where
    freePositions = emptySquares board

-- A move is valid if it traps at least one opponent piece
validMove :: Move -> Board.Board -> Bool
validMove (Move color (x, y)) board
  | isNothing (getRowOf board (x, y)) || isNothing (getColOf board (x, y)) = False
  | otherwise = trapsInRow || trapsInCol
  where
    trapsInRow = causesFlipInUnit color x row
    trapsInCol = causesFlipInUnit color y col
    Just row = getRowOf board (x, y)
    Just col = getColOf board (x, y)

-- disregard nothing's for now
-- just check if there is an opponent piece to the left or right in the list
-- that is sandwiched between the piece being placed at index and another piece of the same colour
causesFlipInUnit :: Piece -> Int -> [Square] -> Bool
causesFlipInUnit _ _ [] = False
causesFlipInUnit color index squares = trapsLeft || trapsRight
  where
    trapsLeft = checkLeft color index squares
    trapsRight = checkRight color index squares

checkLeft :: Piece -> Int -> [Square] -> Bool
checkLeft color index squares = playerPieceExists && leftMostPlayerIndex < adjacentIndex && squares !! adjacentIndex == Just (opponentColor color)
  where
    (playerPieceExists, leftMostPlayerIndex) = firstIndex color squares
    adjacentIndex = index - 1

-- this is the one which does not work!
checkRight :: Piece -> Int -> [Square] -> Bool
checkRight color index squares = playerPieceExists && rightMostPlayerIndex > adjacentIndex && squares !! adjacentIndex == Just (opponentColor color)
  where
    (playerPieceExists, reversedIndex) = firstIndex color $ reverse squares -- This will not produce the correct index, will give us the index in the reversed list, have to remove that much from the right-index
    adjacentIndex = index + 1
    rightMostPlayerIndex = length squares - (1 + reversedIndex)

firstIndex :: Piece -> [Square] -> (Bool, Int)
firstIndex color squares = firstIndex' color squares 0

firstIndex' :: Piece -> [Square] -> Int -> (Bool, Int)
firstIndex' _ [] _ = (False, -1)
firstIndex' color (Nothing : sqs) index = firstIndex' color sqs (index + 1) -- The way this is used in flippingMove should not allow a Nothing to appear!!
firstIndex' color ((Just piece) : sqs) index = (True, index)
