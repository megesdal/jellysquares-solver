module Test.Main where

import Main (GameBoard(Rectangle), isComplete)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, print)
import Prelude (Unit, ($))

board :: GameBoard
board =
  Rectangle 3 3
    [ GameTile (Goal Green) (Jelly Green Up)
    , GameTile Blank Empty
    , GameTile Blank Empty
    , GameTile Blank Empty
    , GameTile Blank (Jelly Blue Down)
    , GameTile Blank Empty
    , GameTile Blank Empty
    , GameTile Blank Empty
    , GameTile Blank Empty
    ]


main :: forall eff. Eff ( console :: CONSOLE | eff ) Unit
main = do
  --print $ numColors board
  print $ isComplete board
