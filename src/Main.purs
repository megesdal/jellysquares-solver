module Main where


import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, print)
import Prelude
  ( Unit
  , ($)
  , bind
  )

import JellySquares
import JellySquares.Solver (solve)


board1 :: GameBoard
board1 =
  let
    tiles =
      [ Empty Blank
      , Empty Blank
      , Empty Blank

      , Empty Blank
      , Empty (Goal Red)
      , Empty Blank

      , Empty Blank
      , Empty Blank
      , Empty Blank

      , Empty Blank
      , Occupied Blank (Jelly Red Up)
      , Empty Blank

      , Empty Blank
      , Empty Blank
      , Empty Blank
      ]
  in
    createFrom 3 tiles (Empty Blank)


board2 :: GameBoard
board2 =
  let
    tiles =
      [ Empty Blank
      , Empty Blank
      , Empty Blank

      , Empty Blank
      , Empty Blank
      , Occupied Blank (Jelly Green Down)

      , Empty (Goal Blue)
      , Empty Blank
      , Empty (Goal Green)

      , Occupied Blank (Jelly Blue Up)
      , Empty Blank
      , Empty Blank

      , Empty Blank
      , Empty Blank
      , Empty Blank
      ]
  in
    createFrom 3 tiles (Empty Blank)


board44 :: GameBoard
board44 =
  let
    tiles =
      [ Empty Blank
      , Empty Blank
      , Empty Blank
      , Empty Blank

      , Empty Blank
      , Empty (Arrow Down)
      , Empty (Goal Blue)
      , Empty Blank

      , Empty (Goal Purple)
      , Occupied Blank (Jelly Blue Up)
      , Occupied Blank (Jelly Red Right)
      , Empty (Arrow Left)

      , Empty (Arrow Right)
      , Occupied Blank (Jelly Purple Left)
      , Occupied Blank (Jelly Green Down)
      , Empty (Goal Red)

      , Empty Blank
      , Empty (Goal Green)
      , Empty (Arrow Up)
      , Empty Blank

      , Empty Blank
      , Empty Blank
      , Empty Blank
      , Empty Blank
      ]
  in
    createFrom 4 tiles (Empty Blank)


main :: forall eff. Eff ( console :: CONSOLE | eff ) Unit
main = do
  print board1
  print $ possibleMoves board2
  print $ solve board2
