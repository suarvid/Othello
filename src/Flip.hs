module Flip where
import Board(Piece(White, Black), Square)
import Data.List.Split
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

-- should be enough to get the max and min indexes of each colour
-- if the maximum of either colour is greater than the minimum of the other,
-- and the minimum of that colour is less than the maximum

whiteIndexes :: [Square] -> [Int]
whiteIndexes = pieceIndexes White 0

blackIndexes :: [Square] -> [Int]
blackIndexes = pieceIndexes Black 0

pieceIndexes :: Piece -> Int -> [Square] -> [Int]
pieceIndexes _ _ [] = []
pieceIndexes White start (Just White : ps) = start : pieceIndexes White (start+1) ps
pieceIndexes Black start (Just Black : ps) = start : pieceIndexes Black (start+1) ps
pieceIndexes White start (_:ps) = pieceIndexes White (start+1) ps
pieceIndexes Black start (_:ps) = pieceIndexes Black (start+1) ps
-- an empty row is filled with Nothing
emptyRow :: [Square] -> Bool
emptyRow = all (== Nothing)

splitOnEmpty :: [Square] -> [[Square]]
splitOnEmpty = wordsBy (== Nothing)

-- requires that no empty squares are present
squareToPiece :: [Square] -> [Piece]
squareToPiece [] = []
squareToPiece (Just p : ps) = p : squareToPiece ps
squareToPiece (Nothing : ps) = error "Cannot turn an empty Square into a piece"

-- assumes no empty spaces in the given row
betweenOtherColor :: Int -> [Int] -> Bool
betweenOtherColor 0 _ = False
betweenOtherColor 7 _ = False
betweenOtherColor x ys = any (<x) ys && any (>x) ys

flipColour :: [Piece] -> [Bool] -> [Piece]
flipColour [] _ = []
flipColour _ [] = []
flipColour (p:ps) (False:bs) = p : flipColour ps bs
flipColour (White:ps) (True:bs) = Black : flipColour ps bs
flipColour (Black:ps) (True:bs) = White : flipColour ps bs
