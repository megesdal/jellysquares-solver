module Main where


import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, print)
import Prelude (Unit, ($), (+), (==), class Eq, class Show)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Array ((!!))


data Color
  = Red
  | Yellow
  | Green
  | Blue


instance showColor :: Show Color where
  show Red = "red"
  show Yellow = "yellow"
  show Green = "green"
  show Blue = "blue"


instance eqColor :: Eq Color where
  eq Red Red = true
  eq Yellow Yellow = true
  eq Green Green = true
  eq Blue Blue = true
  eq _ _ = false


data GameTileType
  = Blank
  | ArrowUp
  | ArrowRight
  | ArrowDown
  | ArrowLeft
  | Goal Color


data GameTile = GameTile GameTileType (Maybe Color)


type GameBoard = Array (Array GameTile)


numColors :: GameBoard -> Int
numColors gameBoard =
  let
    recNumColors :: Int -> Int -> Int -> Int
    recNumColors rowIdx colIdx count =
      case gameBoard !! rowIdx of
        (Just row) ->
          case row !! colIdx of
            (Just (GameTile _ (Just _))) ->
              recNumColors rowIdx (colIdx + 1) (count + 1)

            (Just _) ->
              recNumColors rowIdx (colIdx + 1) count

            Nothing ->
              recNumColors (rowIdx + 1) 0 count

        Nothing ->
          count
  in
    recNumColors 0 0 0



isComplete :: GameBoard -> Boolean
isComplete gameBoard =
  let
    numNeeded = numColors gameBoard

    recIsComplete :: Int -> Int -> Int -> Boolean
    recIsComplete rowIdx colIdx numFound =
      if numNeeded == numFound then
        true
      else
        case gameBoard !! rowIdx of
          (Just row) ->
            case row !! colIdx of
              (Just (GameTile (Goal c1) (Just c2))) ->
                if c1 == c2 then
                  recIsComplete rowIdx (colIdx + 1) (numFound + 1)
                else
                  recIsComplete rowIdx (colIdx + 1) numFound

              (Just _) ->
                recIsComplete rowIdx (colIdx + 1) numFound

              Nothing ->
                recIsComplete (rowIdx + 1) 0 numFound

          Nothing ->
            false
  in
    recIsComplete 0 0 0


board :: GameBoard
board =
    [ [ GameTile (Goal Green) (Just Green)
      , GameTile Blank Nothing
      , GameTile Blank Nothing
      ]
    , [ GameTile Blank Nothing
      , GameTile (Goal Blue) (Just Blue)
      , GameTile Blank Nothing
      ]
    , [ GameTile Blank Nothing
      , GameTile Blank Nothing
      , GameTile Blank Nothing
      ]
    ]


main :: forall eff. Eff ( console :: CONSOLE | eff ) Unit
main = do
  --print $ numColors board
  print $ isComplete board
