module JellySquares.Solver
  ( solve
  , Move
  , Solution
  ) where

import Prelude
  ( (>>=)
  , (<$>)
  , ($)
  , (+)
  , (==)
  , (>)
  , (<)
  , (&&)
  , (++)
  , class Show
  , class Eq
  , class Ord
  , Ordering(..)
  , show
  , flip
  )
import Data.Tuple (Tuple(..))
import Data.List (List(..), (:), head, tail, length)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Set (Set)
import Data.Set as Set
import JellySquares
  ( GameBoard
  , jellyAt
  , possibleMoves
  , isComplete
  , move
  , distanceFromSolution
  )
import BinomialHeap
import Heap (empty, deleteMin, insertList)


import Debug.Trace
  ( trace
  , traceShow
  )


-- starting board and the move played on it
data Move = Move GameBoard Int Int (List Move)

{-
path :: Move -> List Move
path move@(Move _ _ _ pathIn) =
  move : pathIn
-}

perform :: Move -> Maybe (Tuple GameBoard (List Move))
perform move@(Move gameBoard row col path) =
  flip Tuple (move : path) <$> JellySquares.move row col gameBoard


{-
performList :: List Move -> Maybe (Tuple GameBoard (List Move))
performList Nil = Nothing
performList (Cons move Nil) = perform move
performList (Cons _ rest) = perform move >>= \_ ->
  case perform move of
    Nothing -> Nothing
    (Just (Tuple nextBoard _)) ->
      case rest of
        Nil ->
-}


instance moveShow :: Show Move where
  show (Move board row col _) =
    case jellyAt row col board of
      Nothing -> ":("
      Just tile -> show tile


instance moveEq :: Eq Move where
  eq (Move board1 row1 col1 path1) (Move board2 row2 col2 path2) =
    board1 == board2 && row1 == row2 && col1 == col2


instance moveOrd :: Ord Move where
  --compare :: Move -> Move -> Ordering
  compare (Move board1 row1 col1 path1) (Move board2 row2 col2 path2) =
    let
      dist1 = distanceFromSolution board1
      dist2 = distanceFromSolution board2
    in
      if dist1 < dist2 then
        LT
      else if dist1 > dist2 then
        GT
      else
        let
          len1 = length path1
          len2 = length path2
        in
          if len1 < len2 then
            LT
          else if len1 > len2 then
            GT
          else
            EQ


newtype Solution = Solution (List Move)


showMoves :: List Move -> String
showMoves moves =
  let
    recShow Nil str = str
    recShow (Cons x xs) str =
      recShow xs $ " -> " ++ show x ++ str

    showDistance =
      case head moves of
        Nothing -> ""
        Just (Move gameBoard _ _ _) ->
          show $ distanceFromSolution gameBoard
  in
    showDistance ++ (recShow moves "")


instance showSolution :: Show Solution where
  show (Solution moves) = showMoves moves


seenBefore :: GameBoard -> List Move -> Boolean
seenBefore gameBoard moves =
  let
    recSeenBefore movesLeft =
      case head movesLeft of
        Nothing ->
          false

        Just (Move moveGameBoard _ _ _) ->
          if gameBoard == moveGameBoard then
            true
          else
            recSeenBefore
              case tail movesLeft of
                Just xs -> xs
                Nothing -> Nil
  in
    recSeenBefore moves


findPossibleNextMoves :: Move -> List Move
findPossibleNextMoves moveToDo =
  case perform moveToDo of
    Nothing -> Nil

    Just (Tuple nextBoard movePath) ->
      if seenBefore nextBoard movePath then
        --trace ("CYCLE!! (skipping)") \_ ->
        Nil
      else
        findMovesOnBoard movePath nextBoard


findMovesOnBoard :: List Move -> GameBoard -> List Move
findMovesOnBoard pathIn currentBoard =
  let
    createMove (Tuple row col) =
      Move currentBoard row col pathIn
  in
    createMove <$> possibleMoves currentBoard


processMoves :: List Move -> List Move -> Either (List Move) Solution
processMoves Nil queue = Left queue
processMoves (Cons currentMove otherMovesToTry) queue =
  case perform currentMove of
    Nothing ->
      processMoves otherMovesToTry queue

    Just (Tuple resultingBoard movePath) ->
      if isComplete resultingBoard then
        Right (Solution movePath)
      else if seenBefore resultingBoard movePath then
        --trace ("CYCLE!! (skipping)") \_ ->
        processMoves otherMovesToTry queue
      else
        let
          movesToQueue =
            findMovesOnBoard
              --trace (showMoves movePath) \_ ->
              movePath
              resultingBoard
        in
          processMoves otherMovesToTry (movesToQueue ++ queue)


recSolveBreadthFirst :: BinomialHeap Move -> Set GameBoard -> Maybe Solution
recSolveBreadthFirst moveQueue boardsVisited =
  case deleteMin moveQueue of
    Nothing ->
      -- queue is empty... I guess we're done
      Nothing

    Just (Tuple nextMove nextQueue) ->
      case perform nextMove of
        Nothing ->
          recSolveBreadthFirst nextQueue boardsVisited

        Just (Tuple resultingBoard movePath) ->
          if isComplete resultingBoard then
            Just (Solution movePath)
          else if Set.member resultingBoard boardsVisited then
            --trace ("CYCLE!! (skipping)") \_ ->
            recSolveBreadthFirst nextQueue boardsVisited
          else
            let
              movesToQueue =
                findMovesOnBoard
                  (trace (showMoves movePath) \_ ->
                    movePath)
                  resultingBoard
            in
              recSolveBreadthFirst
                (insertList movesToQueue nextQueue)
                (Set.insert resultingBoard boardsVisited)


recSolveDepthFirst :: GameBoard -> List Move -> Int -> Maybe Solution
recSolveDepthFirst currentBoard movesSoFar depth =
  if isComplete (trace "===recSolve===" \_->(traceShow currentBoard (\_ -> currentBoard))) then
    Just (Solution movesSoFar)
  else
    let
      recTryPossibles count movesLeftToTry =
        case head movesLeftToTry of
          Nothing ->
            (trace ("dead end! " ++ show depth ++ " " ++ show count) \_ -> Nothing)

          Just (Tuple row col) ->
            case move (trace ("move at " ++ show depth ++ " " ++ show count ++ ": row " ++ (show row) ++ " col " ++ (show col)) \_ -> row) col currentBoard of
              Nothing ->
                (tail movesLeftToTry) >>= recTryPossibles (count + 1)

              Just nextBoard ->
                if seenBefore nextBoard movesSoFar then
                  trace ("CYCLE!! (skipping) " ++ show depth ++ " " ++ show count) \_ ->
                  (tail movesLeftToTry) >>= recTryPossibles (count + 1)
                else
                  case recSolveDepthFirst nextBoard (Move currentBoard row col movesSoFar : movesSoFar) (depth + 1) of
                    Just solution ->
                      Just solution
                    Nothing ->
                      (tail movesLeftToTry) >>= recTryPossibles (count + 1)
    in
      recTryPossibles 0 $ possibleMoves currentBoard


solve :: GameBoard -> Maybe Solution
solve gameBoard =
  recSolveBreadthFirst (insertList (findMovesOnBoard Nil gameBoard) empty) Set.empty
  -- recSolveDepthFirst gameBoard Nil 0
