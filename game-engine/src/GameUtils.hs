module GameUtils where

import qualified Text.Parsec as P
import qualified Text.Parsec.Language as P
import qualified Text.Parsec.Token as P
import qualified Data.Map as M
import Data.List

import GameModel

eitherToJust :: Either a b -> Maybe b
eitherToJust = either (const Nothing) Just

parseNumber :: String -> Maybe Int
parseNumber nStr = fmap fromIntegral $
  eitherToJust $ P.parse (P.decimal P.haskell) "" nStr

neighbouringOrdinates :: Game -> Int -> [Int]
neighbouringOrdinates (Game { gameGridSize = gridSize }) v =
  if v == 0 then
    [v, v + 1]
  else if v == gridSize - 1 then
    [v - 1, v]
  else
    [v - 1, v, v + 1]

neighboursOfPlusSelf :: Game -> Coord -> [Coord]
neighboursOfPlusSelf game (Coord x y) =
    [Coord xNeigh yNeigh | xNeigh <- xNeighbours, yNeigh <- yNeighbours]
  where
    xNeighbours = neighbouringOrdinates game x
    yNeighbours = neighbouringOrdinates game y

neighboursOf :: Game -> Coord -> [Coord]
neighboursOf g c = filter (\c' -> c' /= c) $ neighboursOfPlusSelf g c

gameToMineCounts :: Game -> M.Map Coord Int
gameToMineCounts g@(Game _ status) =
    M.mapWithKey countForMine nonMines
  where
    nonMines = M.filter (\(CellStatus _ s) -> s == CellNotMine) status
    isMine coord = case M.lookup coord status of
      Just (CellStatus _ CellMine) -> True
      _ -> False
    countForMine coord _ = length $ filter isMine $ neighboursOf g coord

-- If the are any known cells with no mine neighbours, auto-expose their neighbours
-- to follow the standard minesweeper rule...
expandNotMinesWithoutNeighbours :: Game -> M.Map Coord Int -> Game
expandNotMinesWithoutNeighbours g mineCounts =
    case unknownNeighboursOfZeroCandidates of
      [] -> g
      _ -> expandNotMinesWithoutNeighbours newGame mineCounts
  where
    coordsWithZeroCount = M.keys . M.filter (\x -> x == 0) $ mineCounts
    isKnown coord = case M.lookup coord (gameStatus g) of
      Just (CellStatus CellKnown _) -> True
      _ -> False
    knownZeroCandidates = filter isKnown coordsWithZeroCount
    unknownNeighboursOfZeroCandidates = concatMap
      (\coord -> filter (not . isKnown) $ neighboursOf g coord) knownZeroCandidates
    expandOneMine status coord =
      M.insert coord (CellStatus CellKnown CellNotMine) status
    newGame :: Game
    newGame = g { gameStatus =
                  foldl' expandOneMine (gameStatus g) unknownNeighboursOfZeroCandidates
                }
