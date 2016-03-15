module Main where


import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, print)
import Prelude
  ( class Eq
  , class Show
  , Unit
  , ($)
  , (+)
  , (-)
  , (*)
  , mod
  , (==)
  , (>>=)
  , (<$>)
  , (++)
  , show
  , bind
  )
import Data.Maybe (Maybe(Nothing, Just))
import Data.Array ((!!), updateAt)


data Color
  = Red
  | Purple
  | Green
  | Blue


instance showColor :: Show Color where
  show Red = "r"
  show Purple = "p"
  show Green = "g"
  show Blue = "b"


instance eqColor :: Eq Color where
  eq Red Red = true
  eq Purple Purple = true
  eq Green Green = true
  eq Blue Blue = true
  eq _ _ = false


data Direction
  = Up
  | Down
  | Left
  | Right


instance showDirection :: Show Direction where
  show Up = "U"
  show Down = "D"
  show Left = "L"
  show Right = "R"


data GameTileType
  = Blank
  | Arrow Direction
  | Goal Color


instance showGameTileType :: Show GameTileType where
  show Blank = "-"
  show (Arrow direction) = show direction
  show (Goal color) = show color


data Jelly = Jelly Color Direction | Empty


instance showJelly :: Show Jelly where
  show (Jelly color direction) = (show color) ++ (show direction)
  show Empty = "--"


data GameTile = GameTile GameTileType Jelly


instance showGameTile :: Show GameTile where
  show (GameTile gameTileType jelly) =
    (show gameTileType) ++ "|" ++ (show jelly)


-- data plus nrows and ncols... only ncols needed
data GameBoard = Rectangle Int Int (Array GameTile)


instance showGameBoard :: Show GameBoard where
  show (Rectangle nrows ncols tiles) =
    let
      recPrintTiles :: String -> Int -> String
      recPrintTiles tilesStr tileIdx =
        case tiles !! tileIdx of
          Just tile ->
            recPrintTiles
              (tilesStr ++ (if tilesStr == "" then "" else if mod tileIdx ncols == 0 then "\n" else "  ") ++ (show tile))
              (tileIdx + 1)
          Nothing ->
            tilesStr
    in
      recPrintTiles "" 0


tileAt :: Int -> Int -> GameBoard -> Maybe GameTile
tileAt rowIdx colIdx (Rectangle nrows ncols tiles) =
  tiles !! (rowIdx * ncols + colIdx)  -- >>= (flip (!!) colIdx)


placeAt :: Int -> Int -> Jelly -> GameBoard -> Maybe GameBoard
placeAt rowIdx colIdx jelly gameBoard =
  let
    replaceTile :: GameBoard -> GameTile -> Maybe GameBoard
    replaceTile (Rectangle nrows ncols tiles) (GameTile existingType _) =
      let
        nextJelly =
          case jelly of
            Jelly color _ ->
              case existingType of
                Arrow direction -> Jelly color direction
                _ -> jelly
            Empty -> Empty

        maybeNextTiles =
          updateAt (rowIdx * ncols + colIdx) (GameTile existingType nextJelly) tiles
      in
        Rectangle nrows ncols <$> maybeNextTiles
  in
    tileAt rowIdx colIdx gameBoard >>=
      replaceTile gameBoard


reduce :: forall a. GameBoard -> a -> (a -> GameTile -> a) -> a
reduce (Rectangle nrows ncols tiles) start fn =
  let
    recReduce :: a -> Int -> a
    recReduce last tileIdx =
      case tiles !! tileIdx of
        Just tile ->
          recReduce (fn last tile) (tileIdx + 1)
        Nothing ->
          last
  in
    recReduce start 0


numColors :: GameBoard -> Int
numColors gameBoard =
  let
    countJelly :: Int -> GameTile -> Int
    countJelly count tile =
      case tile of
        GameTile _ (Jelly _ _) ->
          count + 1

        _ ->
          count
  in
    reduce gameBoard 0 countJelly


isComplete :: GameBoard -> Boolean
isComplete gameBoard =
  let
    countJellyOnGoal :: Int -> GameTile -> Int
    countJellyOnGoal count tile =
      case tile of
        GameTile (Goal c1) (Jelly c2 _) ->
          if c1 == c2 then
            count + 1
          else
            count

        _ ->
          count
  in
    (reduce gameBoard 0 countJellyOnGoal) == (numColors gameBoard)


recMove ::  Direction -> Int -> Int  -> GameBoard -> Maybe GameBoard
recMove direction fromRow fromCol gameBoard =
  let
    toRow =
      case direction of
        Up -> fromRow - 1
        Down -> fromRow + 1
        _ -> fromRow

    toCol =
      case direction of
        Right -> fromCol + 1
        Left -> fromCol - 1
        _ -> fromCol
  in
    case tileAt toRow toCol gameBoard of
      -- off the board... no can
      Nothing ->
        Nothing

      -- moving to an empty square is okay
      Just (GameTile _ Empty) ->
        tileAt fromRow fromCol gameBoard >>=
          \(GameTile _ jelly) -> placeAt toRow toCol jelly gameBoard >>=
            placeAt fromRow fromCol Empty

      -- gotta move the existing tile first
      Just (GameTile _ (Jelly _ _)) ->
        recMove direction toRow toCol gameBoard >>=
          recMove direction fromRow fromCol


move ::  Int -> Int -> GameBoard -> Maybe GameBoard
move rowIdx colIdx gameBoard =
  case tileAt rowIdx colIdx gameBoard of
    Just (GameTile _ (Jelly _ direction)) ->
      recMove direction rowIdx colIdx gameBoard

    -- off the board... no can
    Nothing ->
      Nothing


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
