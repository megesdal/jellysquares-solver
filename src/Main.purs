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

      , Empty (Goal Red)

      , Empty Blank

      , Occupied Blank (Jelly Red Up)
      ]
  in
    createFrom 1 tiles (Empty Blank)


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


board3 :: GameBoard
board3 =
  let
    tiles =
      [ Empty (Goal Green)
      , Occupied Blank (Jelly Green Left)
      , Occupied Blank (Jelly Red Right)
      , Empty (Goal Red)
      ]
  in
    createFrom 4 tiles (Empty Blank)

board4 :: GameBoard
board4 =
  let
    tiles =
      [ Empty Blank
      , Empty Blank
      , Empty Blank
      , Empty Blank
      , Empty Blank

      , Occupied Blank (Jelly Red Right)
      , Occupied Blank (Jelly Green Up)
      , Empty Blank
      , Empty (Goal Red)
      , Empty (Goal Green)
      ]
  in
    createFrom 5 tiles (Empty Blank)


board5 :: GameBoard
board5 =
  let
    tiles =
      [ Empty Blank
      , Occupied Blank (Jelly Green Down)
      , Empty Blank
      , Empty Blank

      , Occupied Blank (Jelly Red Right)
      , Empty Blank
      , Empty (Goal Red)
      , Empty Blank

      , Empty Blank
      , Empty (Goal Green)
      , Empty Blank
      , Empty Blank
      ]
  in
    createFrom 4 tiles (Empty Blank)


board6 :: GameBoard
board6 =
  let
    tiles =
      [ Empty (Goal Red)
      , Empty Blank

      , Empty Blank
      , Empty (Goal Green)

      , Occupied Blank (Jelly Green Right)
      , Empty Blank

      , Occupied Blank (Jelly Red Up)
      , Empty Blank
      ]
  in
    createFrom 2 tiles (Empty Blank)

board7 :: GameBoard
board7 =
  let
    tiles =
      [ Empty Blank
      , Empty Blank
      , Occupied Blank (Jelly Red Down)
      , Empty Blank

      , Empty (Goal Green)
      , Empty Blank
      , Empty Blank
      , Occupied Blank (Jelly Green Left)

      , Empty Blank
      , Empty (Goal Red)
      , Empty Blank
      , Empty Blank
      ]
  in
    createFrom 4 tiles (Empty Blank)


board8 :: GameBoard
board8 =
  let
    tiles =
      [ Empty Blank
      , Empty Blank
      , Empty Blank

      , Empty Blank
      , Empty Blank
      , Empty Blank

      , Empty (Goal Green)
      , Empty (Goal Red)
      , Empty (Goal Blue)

      , Empty Blank
      , Occupied Blank (Jelly Blue Right)
      , Empty Blank

      , Empty Blank
      , Occupied Blank (Jelly Green Left)
      , Empty Blank

      , Empty Blank
      , Occupied Blank (Jelly Red Up)
      , Empty Blank
      ]
  in
    createFrom 3 tiles (Empty Blank)


board9 :: GameBoard
board9 =
  let
    tiles =
      [ Empty Blank
      , Empty Blank
      , Empty Blank

      , Empty Blank
      , Occupied Blank (Jelly Green Down)
      , Empty (Goal Blue)

      , Occupied Blank (Jelly Purple Right)
      , Empty (Goal Green)
      , Empty (Goal Purple)

      , Empty Blank
      , Empty Blank
      , Occupied Blank (Jelly Blue Up)

      , Empty Blank
      , Empty Blank
      , Empty Blank
      ]
  in
    createFrom 3 tiles (Empty Blank)

board10 :: GameBoard
board10 =
  let
    tiles =
      [ Empty Blank
      , Occupied Blank (Jelly Red Right)
      , Occupied Blank (Jelly Green Down)
      , Empty (Goal Red)
      , Empty Blank

      , Empty Blank
      , Empty Blank
      , Empty Blank
      , Occupied Blank (Jelly Blue Left)
      , Empty Blank

      , Empty Blank
      , Empty Blank
      , Empty (Goal Blue)
      , Empty (Goal Green)
      , Empty Blank
      ]
  in
    createFrom 5 tiles (Empty Blank)


board11 :: GameBoard
board11 =
  let
    tiles =
      [ Empty Blank
      , Occupied Blank (Jelly Blue Down)
      , Empty Blank
      , Empty Blank

      , Empty Blank
      , Empty (Goal Red)
      , Empty Blank
      , Occupied Blank (Jelly Red Left)

      , Empty Blank
      , Empty (Goal Green)
      , Empty Blank
      , Empty Blank

      , Empty (Goal Blue)
      , Empty Blank
      , Occupied Blank (Jelly Green Up)
      , Empty Blank
      ]
  in
    createFrom 4 tiles (Empty Blank)

board12 :: GameBoard
board12 =
  let
    tiles =
      [ Empty Blank
      , Empty (Goal Blue)
      , Empty Blank
      , Empty Blank
      , Empty Blank

      , Occupied Blank (Jelly Green Right)
      , Empty Blank
      , Occupied Blank (Jelly Blue Left)
      , Empty (Goal Green)
      , Empty (Goal Red)

      , Empty Blank
      , Empty Blank
      , Empty Blank
      , Occupied Blank (Jelly Red Up)
      , Empty Blank
      ]
  in
    createFrom 5 tiles (Empty Blank)


board13 :: GameBoard
board13 =
  let
    tiles =
      [ Empty (Goal Green)
      , Empty Blank
      , Empty Blank
      , Empty Blank
      , Empty Blank

      , Empty Blank
      , Empty Blank
      , Empty Blank
      , Empty Blank
      , Empty (Goal Red)

      , Empty (Goal Blue)
      , Occupied Blank (Jelly Red Right)
      , Empty Blank
      , Occupied Blank (Jelly Blue Left)
      , Empty Blank

      , Empty Blank
      , Empty Blank
      , Empty Blank
      , Empty Blank
      , Empty Blank


      , Empty Blank
      , Occupied Blank (Jelly Green Up)
      , Empty Blank
      , Empty Blank
      , Empty Blank
      ]
  in
    createFrom 5 tiles (Empty Blank)

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
  print board13
  print $ solve board13
