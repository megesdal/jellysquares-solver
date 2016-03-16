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
import JellySquares.Solver (solve)


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
    case createFrom 4 tiles of
      Just x -> x
      Nothing -> create 6 4 (Empty Blank)


main :: forall eff. Eff ( console :: CONSOLE | eff ) Unit
main = do
  print board44
  print $ possibleMoves board44
  print $ solve board44
