module JellySquares
  ( GameBoard
  , Rectangle
  , Positioned
  , GameTile(..)
  , GameTileType(..)
  , Direction(..)
  , Color(..)
  , Jelly(..)
  , create
  , createFrom
  , move
  , isComplete
  , possibleMoves
  , jellyAt
  ) where


import Prelude
  ( class Eq
  , class Show
  , (+)
  , (-)
  , (*)
  , (/)
  , mod
  , (==)
  , (<)
  , (>=)
  , (&&)
  , (>>=)
  , bind
  , ($)
  , (<$>)
  , (++)
  , show
  )
import Data.Maybe (Maybe(Nothing, Just))
import Data.Array ((!!), updateAt, zipWith, snoc)
import Data.Foldable (class Foldable, and, foldl, foldr)
import Data.Tuple
import Data.List (List(Nil), (:))
import Data.Monoid (mempty)


-- Color --
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


-- Direction --
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


instance eqDirection :: Eq Direction where
  eq Up Up = true
  eq Down Down = true
  eq Left Left = true
  eq Right Right = true
  eq _ _ = false


-- GameTileType --
data GameTileType
  = Blank
  | Arrow Direction
  | Goal Color


instance showGameTileType :: Show GameTileType where
  show Blank = "-"
  show (Arrow direction) = show direction
  show (Goal color) = show color


instance eqGameTileType :: Eq GameTileType where
  eq Blank Blank = true
  eq (Arrow direction1) (Arrow direction2) = direction1 == direction2
  eq (Goal color1) (Goal color2) = color1 == color2
  eq _ _ = false


-- Jelly --
data Jelly = Jelly Color Direction


colorOf :: Jelly -> Color
colorOf (Jelly color _) = color


directionOf :: Jelly -> Direction
directionOf (Jelly _ direction) = direction


instance showJelly :: Show Jelly where
  show (Jelly color direction) = (show color) ++ (show direction)


instance eqJelly :: Eq Jelly where
  eq (Jelly color1 direction1) (Jelly color2 direction2) =
    color1 == color2 && direction1 == direction2


-- GameTile
data GameTile = Occupied GameTileType Jelly | Empty GameTileType


typeOf :: GameTile -> GameTileType
typeOf (Occupied gameTileType _) = gameTileType
typeOf (Empty gameTileType) = gameTileType


instance showGameTile :: Show GameTile where
  show (Occupied gameTileType jelly) =
    (show gameTileType) ++ "|" ++ (show jelly)
  show (Empty gameTileType) =
    (show gameTileType) ++ "|--"


instance eqGameTile :: Eq GameTile where
  eq (Occupied gameTileType1 jelly1) (Occupied gameTileType2 jelly2) =
    gameTileType1 == gameTileType2 && jelly1 == jelly2
  eq (Empty gameTileType1) (Empty gameTileType2) =
    gameTileType1 == gameTileType2
  eq _ _ =
    false


-- Positioned a
data Positioned a = Positioned Int Int a


unwrap :: forall a. Positioned a -> a
unwrap (Positioned _ _ x) = x


instance eqPositioned :: (Eq a) => Eq (Positioned a) where
  eq (Positioned row1 col1 x1) (Positioned row2 col2 x2) =
    row1 == row2 && col1 == col2 && x1 == x2


-- Rectangle
data Rectangle a = Rectangle Int (Array a)


create :: forall a. Int -> Int -> a -> Rectangle (Positioned a)
create nrows ncols value =
  let
    recCreate :: Array (Positioned a) -> Int -> Array (Positioned a)
    recCreate positionedValues i =
      if i < nrows * ncols then
        recCreate
          (snoc positionedValues (Positioned (i / ncols) (mod i ncols) value))
          (i + 1)
      else
        positionedValues
  in
    Rectangle ncols $ recCreate [] 0


createFrom :: forall a. Int -> Array a -> a -> Rectangle (Positioned a)
createFrom ncols values defaultValue =
  let
    recCreate :: Array (Positioned a) -> Int -> Array (Positioned a)
    recCreate positionedValues i =
      case values !! i of
        Just value ->
          recCreate
            (snoc positionedValues (Positioned (i / ncols) (mod i ncols) value))
            (i + 1)
        Nothing ->
          if mod i ncols == 0 then
            positionedValues
          else
            recCreate
              (snoc positionedValues (Positioned (i / ncols) (mod i ncols) defaultValue))
              (i + 1)
  in
    Rectangle ncols (recCreate [] 0)


updateTileAt :: forall a. Int -> Int -> a -> Rectangle (Positioned a) -> Maybe (Rectangle (Positioned a))
updateTileAt row col tile (Rectangle ncols tiles) =
  Rectangle ncols <$> updateAt (row * ncols + col) (Positioned row col tile) tiles


tileAt :: forall a. Int -> Int -> Rectangle (Positioned a) -> Maybe a
tileAt rowIdx colIdx (Rectangle ncols tiles) =
  if colIdx >= ncols then
    Nothing
  else
    case tiles !! (rowIdx * ncols + colIdx) of
      Just (Positioned _ _ tile) -> Just tile
      Nothing -> Nothing


instance showRectangle :: (Show a) => Show (Rectangle (Positioned a)) where
  show (Rectangle ncols values) =
    let
      append acc (Positioned _ col value) =
        acc ++
        (if acc == "" then "" else if col == 0 then "\n" else "  ") ++
        (show value)
    in
      foldl append "" values


instance eqRectangle :: (Eq a) => Eq (Rectangle a) where
  eq (Rectangle ncols1 tiles1) (Rectangle ncols2 tiles2) =
    (and $ zipWith (\a b -> a == b) tiles1 tiles2)
      && ncols1 == ncols2


foldrRectangle :: forall a b. (a -> b -> b) -> b -> Rectangle a -> b
foldrRectangle fn start (Rectangle ncols tiles) =
    foldr fn start tiles


foldlRectangle :: forall a b. (b -> a -> b) -> b -> Rectangle a -> b
foldlRectangle fn start (Rectangle ncols tiles) =
    foldl fn start tiles


instance foldableRectangle :: Foldable Rectangle where
  foldr = foldrRectangle
  foldl = foldlRectangle
  foldMap f xs = foldr (\x acc -> f x ++ acc) mempty xs


-- GameBoard --
type GameBoard = Rectangle (Positioned GameTile)


typeAt :: Int -> Int -> GameBoard -> Maybe GameTileType
typeAt row col gameBoard = typeOf <$> tileAt row col gameBoard


jellyAt :: Int -> Int -> GameBoard -> Maybe Jelly
jellyAt row col gameBoard = do
  tile <- tileAt row col gameBoard
  case tile of
    Empty _ -> Nothing
    Occupied _ jelly -> Just jelly


placeAt :: Int -> Int -> Jelly -> GameBoard -> Maybe GameBoard
placeAt row col jelly gameBoard =
  let
    placeWithType existingType =
      let
        nextJelly =
          case existingType of
            Arrow direction -> Jelly (colorOf jelly) direction
            _ -> jelly
      in
        updateTileAt row col (Occupied existingType nextJelly) gameBoard
  in
    typeAt row col gameBoard >>= placeWithType


clearAt :: Int -> Int -> GameBoard -> Maybe GameBoard
clearAt row col gameBoard =
  let
    placeWithType existingType =
      updateTileAt row col (Empty existingType) gameBoard
  in
    typeAt row col gameBoard >>= placeWithType


numColors :: GameBoard -> Int
numColors gameBoard =
  let
    countJelly count (Positioned _ _ tile) =
      case tile of
        Occupied _ _ ->
          count + 1

        Empty _ ->
          count
  in
    foldl countJelly 0 gameBoard


isJellyOnGoal :: GameTileType -> Jelly -> Boolean
isJellyOnGoal (Goal goalColor) (Jelly jellyColor _) = goalColor == jellyColor
isJellyOnGoal _ _ = false


isComplete :: GameBoard -> Boolean
isComplete gameBoard =
  let
    countJellyOnGoal count (Positioned _ _ (Occupied (Goal c1) (Jelly c2 _))) =
      if c1 == c2 then
        count + 1
      else
        count

    countJellyOnGoal count _ =
      count
  in
    (foldl countJellyOnGoal 0 gameBoard) == (numColors gameBoard)


recMove ::  Direction -> Int -> Int  -> Jelly -> GameBoard -> Maybe GameBoard
recMove direction fromRow fromCol jelly gameBoard =
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
      Just (Empty _) ->
        placeAt toRow toCol jelly gameBoard
          >>= clearAt fromRow fromCol

      -- gotta move the existing tile first
      Just (Occupied _ existingJelly) ->
        recMove direction toRow toCol existingJelly gameBoard
          >>= recMove direction fromRow fromCol jelly


move ::  Int -> Int -> GameBoard -> Maybe GameBoard
move row col gameBoard =
  case tileAt row col gameBoard of
    Just (Occupied tileType jelly) ->
      -- I am not sure this is a good idea...
      if isJellyOnGoal tileType jelly then
        Nothing
      else
        recMove (directionOf jelly) row col jelly gameBoard

    -- off the board or tile is empty... no can
    _ ->
      Nothing


canMove :: Int -> Int -> GameBoard -> Boolean
canMove row col gameBoard =
  case move row col gameBoard of
    Just _ -> true
    Nothing -> false


hasMoves :: GameBoard -> Boolean
hasMoves gameBoard =
  let
    checkCanMove otherCanMove (Positioned row col _) =
      if otherCanMove then
        true
      else
        canMove row col gameBoard
  in
    foldl checkCanMove false gameBoard


possibleMoves :: GameBoard -> List (Tuple Int Int)
possibleMoves gameBoard =
  let
    addPossibleMove (Positioned row col _) moves =
      if canMove row col gameBoard then
        Tuple row col : moves
      else
        moves
  in
    foldr addPossibleMove Nil gameBoard
