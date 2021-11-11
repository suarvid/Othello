module AlphaBeta where

-- This file contains the implementation of the minimax-algorithm
-- with alpha-beta pruning.

import Board (Board, Piece (..), Square, opponentColor)
import Move (genValidMoves, makeMove)
type Alpha = Double
type Beta = Double
type Player = Piece -- Player and Piece are basically the same thing, either White or Black

-- going to need one function for evaluating a given board, giving back its expected utility according to our heuristic function

-- start with naive implementation, just the number of pieces of the player's colour
-- remember: Board = [[Square]] i.e. a list of rows
heuristicValue :: Board -> Player -> Double
heuristicValue [] _ = 0.0
heuristicValue b p = sum $ map (colourInRow p) b

-- Calculate the sum of the number of pieces of a given colour in a row of the board
colourInRow :: Player -> [Square] -> Double
colourInRow _ [] = 0.0
colourInRow colour (Nothing : sqs) = colourInRow colour sqs
colourInRow colour ((Just c) : sqs)
  | colour == c = 1.0 + colourInRow colour sqs
  | otherwise = colourInRow colour sqs

-- TODO: Implement this
gameOver :: Board -> Bool
gameOver b = noWhiteMoves && noBlackMoves
  where
    whiteMoves = Move.genValidMoves White b
    blackMoves = Move.genValidMoves Black b
    noWhiteMoves = null whiteMoves
    noBlackMoves = null blackMoves


-- TODO: Understand this better, and re-write the algorithm to make sure I understand it and it works
-- will take the first element satisfying the condition, or the last element if none do (last wont be checked)
takeFirstWithOrLastElem:: (a-> Bool) -> [a] -> a
takeFirstWithOrLastElem cond [x] = x
takeFirstWithOrLastElem cond (x:xs) = if cond x then x else takeFirstWithOrLastElem cond xs

-- have to alter this to take some depth/time into account!
alphabeta :: Player -> Board -> Double
alphabeta _ [] = 0.0
alphabeta player board 
  | player == White = maxValue board (-100) 100
  | otherwise = minValue board (-100) 100
  where
    maxValue :: Board -> Double -> Double -> Double
    maxValue board alpha beta
      | gameOver board = heuristicValue board player 
      | otherwise =
        let
          children = reverse $ generateChildren player board

          getMinimaxAndAlpha :: (Double, Double) -> Board -> (Double, Double) -- explore board and return new best minimax and alpha value
          getMinimaxAndAlpha (bestMinimaxVal, alpha) board =
            let newMinimax = max (bestMinimaxVal)  (minValue board alpha beta)
            in (newMinimax, max alpha newMinimax)

          (bestMinimax, newAlpha) =
            takeFirstWithOrLastElem (\(v, a) -> v >= beta) $
             scanl getMinimaxAndAlpha (-100, alpha) children

        in bestMinimax

    minValue :: Board -> Double -> Double -> Double
    minValue board alpha beta
      | gameOver board = heuristicValue board player
      | otherwise =
        let
          children = reverse $ generateChildren player board
          getMinimaxAndBeta :: (Double, Double) -> Board -> (Double, Double)
          getMinimaxAndBeta (bestMinimaxVal, beta) board =
            let newMinimax = min (bestMinimaxVal) (maxValue board alpha beta)
            in (newMinimax, min beta newMinimax)

          (bestMinimax, newBeta) =
            takeFirstWithOrLastElem (\(v,b) -> v <= alpha) $
             scanl getMinimaxAndBeta (100, beta) children

        in bestMinimax


-- White is always Max
-- Black is always Min
-- Standard minimax without any pruning. Pretty slow, for depth > 6 it is way too slow.
-- Probably too slow around depth of 5 too.
minimax :: Int -> Player -> Board -> Double
minimax 0 p b = heuristicValue b p -- have to add some check for terminal nodes
minimax depth player board
  | player == White = maximum childValues
  | otherwise = minimum childValues
  where
    children = generateChildren player board
    childValues = map (minimax (depth - 1) (opponentColor player)) children

-- generates all possible boards resulting from legal moves in the currenet situation
generateChildren :: Player -> Board -> [Board]
generateChildren _ [] = []
generateChildren player board = map (makeMove board) legalMoves
  where
    legalMoves = genValidMoves player board

-- maybe the nodes will have to contain (Board, Double) tuples for representing
-- and storing the values of the boards as well as the actual boards
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
