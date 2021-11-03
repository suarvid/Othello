module Main where
import Move (makeMove, Move(Move))
import Board (readBoard, showBoard, Piece, readPiece)
import Data.List

emptyRow = "        \n"
exampleBoard = emptyRow ++ emptyRow ++ emptyRow ++ "   wb   \n" ++ "   bw   \n" ++ emptyRow ++ emptyRow ++ emptyRow
readShow :: String
readShow = showBoard (readBoard exampleBoard)


-- move is Move Piece Row Column
parseMove :: String -> Move
parseMove s = 
    let piece = readPiece ps
        row   = read rs
        col   = read cs
    in Move piece row col
    where 
        ps = head s -- same as !! 0
        rs = [s !! 1]
        cs = [s !! 2]



main :: IO ()
main = do
    let initialBoard = readBoard exampleBoard
    putStrLn "Initial Board: "
    putStrLn $ showBoard initialBoard
    putStrLn "Make your move: "
    moveString <- getLine
    let move = parseMove moveString
    let updatedBoard = makeMove initialBoard move
    putStrLn $ showBoard updatedBoard
    

