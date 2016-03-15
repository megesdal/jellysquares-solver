module Main where


import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, print)
import Prelude
  ( Unit
  , ($)
  , bind
  )
import Data.Maybe (Maybe(Nothing, Just))
import JellySquares


board :: GameBoard
board =
  let
    tiles =
      [ Occupied (Goal Green) (Jelly Green Up)
      , Empty Blank
      , Occupied (Goal Red) (Jelly Red Down)
      , Empty Blank
      , Occupied (Goal Blue) (Jelly Blue Down)
      , Empty Blank
      , Empty (Goal Purple)
      , Occupied Blank (Jelly Purple Up)
      , Empty Blank
      ]
  in
    case createFrom 3 tiles of
      Just x -> x
      Nothing -> create 3 3 (Empty Blank)


main :: forall eff. Eff ( console :: CONSOLE | eff ) Unit
main =
  let
    nextBoard =
      case move 2 1 board of
        Just x -> x
        Nothing -> board
  in do
    print board
    print $ isComplete board
    print nextBoard
    print $ isComplete nextBoard
