module Move(
  initialBoard,
  makeMove,
  Move(..), -- export all value constructors
) where
import Board
-- A Move is just adding a piece to a position
data Move = Move Piece Row Column
type Column = Int
type Row = Int

-- same pattern, here: split, update Row, rejoin
makeMove :: Board -> Move -> Board
makeMove b (Move piece x y) = upperRows ++ [updateRow row piece x] ++ lowerRows
  where (upperRows, row:lowerRows) = splitAt y b 

-- split at col, remove first elem in right list, join back together
updateRow :: [Square] -> Piece -> Column -> [Square]
updateRow r p c = xs ++ [Just p] ++ ys 
  where (xs,_:ys) = splitAt c r

-- Perhaps a bit weird that initialBoard is in Move
-- but setting up a Board does require making moves
-- Makes more sense than having makeMove in Board
-- and otherwise we get circular dependencies
initialBoard :: Board
initialBoard = makeMove (makeMove (makeMove (makeMove Board.emptyBoard (Move Black 3 3)) (Move White 3 4)) (Move White 4 3)) (Move Black 4 4)

-- parse a String of the form "w00" which means white at (0,0) 
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

-- A move is valid if it traps at least one opponent piece
validMove :: Move -> Board.Board -> Bool
validMove = undefined

-- A unit is a row, column or diagonal
trapsInUnit :: Piece -> Int -> [Square] -> Bool
trapsInUnit _ _ [] = False
trapsInUnit _  0 ((Just _):sqs) = False -- position is occupied
trapsInUnit color index (sq:sqs)
  | index > 0 = trapsInUnit color (index-1) (sq:sqs)
  | otherwise = flippingMove color (sq:sqs)

flippingMove :: Piece -> [Square] -> Bool
flippingMove color squares = (opponentExists && playerExists) && (opponentIndex < playerIndex)
  where 
    (opponentExists, opponentIndex) = firstIndex (opponentColor color) squares
    (playerExists, playerIndex) = firstIndex color squares

firstIndex :: Piece -> [Square] -> (Bool, Int)
firstIndex color squares = firstIndex' color squares 0

firstIndex' :: Piece -> [Square] -> Int -> (Bool, Int)
firstIndex' _ [] _ = (False, -1)
firstIndex' color (Nothing:sqs) index = (False, -1) -- firstIndex' color sqs (index+1) -- The way this is used in flippingMove should not allow a Nothing to appear!!
firstIndex' color ((Just piece):sqs) index = (True, index)