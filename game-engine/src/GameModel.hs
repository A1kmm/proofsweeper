module GameModel where

import qualified Data.Map as M

data Coord = Coord Int Int deriving (Eq, Ord, Show)
data CellMineStatus = CellMine | CellNotMine deriving (Eq, Ord, Show)
data CellVisibility = CellKnown | CellUnknown deriving (Eq, Ord, Show)
data CellStatus = CellStatus {cellVisibility :: CellVisibility, cellMineStatus :: CellMineStatus } deriving (Eq, Ord, Show)

data Game = Game {
    gameGridSize :: Int
  , gameStatus :: M.Map Coord CellStatus
                 } deriving (Show)
