module Main where
import Move (genMoves, genValidMoves, makeMove, Move(Move))
import Board (readBoard, showBoard, Piece(..), readPiece, emptyBoard, initialBoard, emptySquares)
import qualified Flip as F
import Data.List

-- have to make a function which takes a board and applies flipping to it

main :: IO ()
main = do
    let board = Board.initialBoard
    putStrLn "Initial Board: "
    putStrLn $ showBoard board
    putStrLn "Valid moves for black: "
    let legalMoves = genValidMoves Black board
    print legalMoves
    let boardOnePly = makeMove board $ head legalMoves
    putStrLn $ showBoard boardOnePly
    let legalMovesOnePly = genValidMoves White boardOnePly
    putStrLn "Legal moves after one move: "
    print legalMovesOnePly


