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
  , (++)
  , class Show
  , show
  )
import Data.Tuple (Tuple(..))
import Data.List (List(..), (:), head, tail)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import JellySquares
  ( GameBoard
  , jellyAt
  , possibleMoves
  , isComplete
  , move
  )

import Debug.Trace
  ( trace
  , traceShow
  )


-- starting board and the move played on it
data Move = Move GameBoard Int Int (List Move)


path :: Move -> List Move
path move =
  let
    previousMoves (Move _ _ _ x) = x
  in
    move : previousMoves move


perform :: Move -> Maybe (Tuple GameBoard (List Move))
perform move =
  let
    doMove (Move gameBoard row col _) =
      JellySquares.move
        --(trace ("move " ++ (show row) ++ " col " ++ (show col)) \_ -> row)
        row
        col
        gameBoard
  in
    (\nextBoard -> Tuple nextBoard (path move)) <$> doMove move


instance moveShow :: Show Move where
  show (Move board row col _) =
    case jellyAt row col board of
      Nothing -> ":("
      Just tile -> show tile


newtype Solution = Solution (List Move)


showMoves :: List Move -> String
showMoves moves =
  let
    recShow Nil str = str
    recShow (Cons x xs) str =
      recShow xs $ " -> " ++ show x ++ str
  in
    recShow moves ""


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


recSolveBreadthFirst :: List Move -> Maybe Solution
recSolveBreadthFirst moves =
  case processMoves moves Nil of
    Left Nil -> Nothing
    Left queue -> recSolveBreadthFirst queue
    Right solution -> Just solution


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
  recSolveBreadthFirst $ findMovesOnBoard Nil gameBoard
  -- recSolveDepthFirst gameBoard Nil 0
