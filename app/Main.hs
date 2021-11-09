module Main where
import Move (initialBoard, makeMove, Move(Move))
import Board (readBoard, showBoard, Piece, readPiece, emptyBoard, emptySquares)
import qualified Flip as F
import Data.List


main :: IO ()
main = do
    let board = Move.initialBoard
    putStrLn "Initial Board: "
    putStrLn $ showBoard board
    putStrLn "Empty positions of board: "
    let emptyIndexes = Board.emptySquares board
    print emptyIndexes


