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
  Rectangle 3 3
    [ GameTile (Goal Green) (Jelly Green Up)
    , GameTile Blank Empty
    , GameTile (Goal Red) (Jelly Red Down)
    , GameTile Blank Empty
    , GameTile (Goal Blue) (Jelly Blue Down)
    , GameTile Blank Empty
    , GameTile (Goal Purple) Empty
    , GameTile Blank (Jelly Purple Up)
    , GameTile Blank Empty
    ]


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
