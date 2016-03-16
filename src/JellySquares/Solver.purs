module JellySquares.Solver
  ( solve ) where

import Prelude ((>>=), ($), (==))
import Data.Tuple (Tuple(..))
import Data.List (List(Nil), (:), head, tail)
import Data.Maybe (Maybe(..))
import JellySquares

import Debug.Trace (trace, traceShow, spy)


-- starting board and the move played on it
type Move = Tuple GameBoard (Tuple Int Int)


seenBefore :: GameBoard -> List Move -> Boolean
seenBefore gameBoard moves =
  let
    recSeenBefore movesLeft =
      case head movesLeft of
        Nothing ->
          false

        Just (Tuple moveGameBoard _) ->
          if gameBoard == moveGameBoard then
            true
          else
            recSeenBefore
              case tail movesLeft of
                Just xs -> xs
                Nothing -> Nil
  in
    recSeenBefore moves


recSolve :: GameBoard -> List Move -> Maybe (List Move)
recSolve currentBoard movesSoFar =
  if isComplete (trace "===recSolve===" \_->(traceShow currentBoard (\_ -> currentBoard))) then
    Just movesSoFar
  else
    let
      recTryPossibles movesLeftToTry =
        case head movesLeftToTry of
          Nothing ->
            Nothing

          Just (Tuple row col) ->
            case move (trace "row" \_->(spy row)) (trace "col" \_->(spy col)) currentBoard of
              Nothing ->
                (tail movesLeftToTry) >>= recTryPossibles

              Just nextBoard ->
                if seenBefore nextBoard movesSoFar then
                  (tail movesLeftToTry) >>= recTryPossibles
                else
                  case recSolve nextBoard ((Tuple currentBoard (Tuple row col)) : movesSoFar) of
                    Just solution ->
                      Just solution
                    Nothing ->
                      (tail movesLeftToTry) >>= recTryPossibles
    in
      recTryPossibles $ possibleMoves currentBoard


solve :: GameBoard -> Maybe (List Move)
solve gameBoard =
  -- TODO: try every possible board until:
  -- 1. the jellies are in their goals
  -- 2. no future boards are left
  -- 3. a previous board has been reached
  recSolve gameBoard Nil



  -- get possible moves
  -- try each one... and record the new boards
  -- if not
