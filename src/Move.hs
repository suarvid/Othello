module Move(
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
