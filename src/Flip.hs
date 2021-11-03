module Flip where
import Board(Piece(White, Black), Square)
-- for determining which pieces to flip in a given board
-- could probably start by only taking the last move made into account
-- and then checking if that results in vertical, horizontal or diagonal lines
-- and then flipping any pieces of the other colour caught between the pieces

-- transposing before calling is probably going to help a lot
-- for reusing code when checking rows/columns

-- check a row for trapped pieces
checkRow :: [Square] -> Piece -> [Square]
checkRow r p = undefined

-- one strategy is to get all white indexes in a row
-- and all black indexes and then compare to see if
-- either colour has any indexes which are lower and
-- higher than any index of the other colour
whiteIndexes :: [Square] -> [Int]
whiteIndexes = pieceIndexes White 0

blackIndexes :: [Square] -> [Int]
blackIndexes = pieceIndexes Black 0

pieceIndexes :: Piece -> Int -> [Square] -> [Int]
pieceIndexes _ _ [] = []
pieceIndexes White start (Just White : ps) = start : pieceIndexes White (start+1) ps
pieceIndexes Black start (Just Black : ps) = start : pieceIndexes Black (start+1) ps

-- an empty row is filled with Nothing
emptyRow :: [Square] -> Bool
emptyRow = all (== Nothing)