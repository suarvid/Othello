module Main where
import Move (genMoves, genValidMoves, initialBoard, makeMove, Move(Move))
import Board (readBoard, showBoard, Piece(..), readPiece, emptyBoard, emptySquares)
import qualified Flip as F
import Data.List


main :: IO ()
main = do
    let board = Move.initialBoard
    putStrLn "Initial Board: "
    putStrLn $ showBoard board
    putStrLn "Valid moves for black: "
    let legalMoves = genValidMoves Black board
    print legalMoves


