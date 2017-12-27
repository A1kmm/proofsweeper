module GameCreation (doNewCommand) where

import qualified Data.Map as M
import GameModel
import GameUtils
import GameIO
import GameDisplay
import IdrisGen
import Control.Monad.Random.Strict

placeMines :: Int -> Game -> Rand StdGen Game
placeMines 0 game = return game
placeMines n (Game { gameGridSize = gridSize, gameStatus = status }) = do
  let nonMines = filter (\(_, CellStatus _ cellStatus) -> cellStatus == CellNotMine) $
                   M.toList status
  mineIdx <- fmap (\v -> v `mod` (length nonMines)) getRandom
  let (mineCoord, _) = nonMines !! mineIdx
  let newGameStatus = status
  placeMines (n-1) (Game gridSize (
                       M.insert mineCoord
                         (CellStatus CellUnknown CellMine)
                         newGameStatus))

mineRatio :: Double
mineRatio = 0.25

generateGame :: Int -> Rand StdGen Game
generateGame gridSize =
    placeMines nMines (Game { gameGridSize = gridSize, gameStatus = blankGrid })
  where
    nMines = ceiling $ fromIntegral (gridSize * gridSize) * mineRatio
    blankGrid = M.fromList [(Coord x y, CellStatus CellUnknown CellNotMine) |
                            x <- [0..(gridSize - 1)], y <- [0..(gridSize - 1)]]

firstClick :: Game -> Rand StdGen Game
firstClick game = do
  let
    mineCounts :: M.Map Coord Int
    mineCounts = gameToMineCounts game
    minMineCounts :: Int
    minMineCounts = minimum (M.elems mineCounts)
  
  let clickOptions = filter (\(coord, st) ->
                              M.lookup coord mineCounts == Just minMineCounts &&
                              st == CellStatus CellUnknown CellNotMine) $
                      M.toList (gameStatus game)
  mineIdx <- fmap (\v -> v `mod` (length clickOptions)) getRandom
  let (mineCoord, _) = clickOptions !! mineIdx
  return $ game { gameStatus = M.insert mineCoord (CellStatus CellKnown CellNotMine) (gameStatus game) }

doNewCommand :: String -> IO ()
doNewCommand nStr =
  case parseNumber nStr of
    Nothing -> putStrLn "Expected number after new"
    Just n | n <= 1 || n > 20 -> putStrLn "Number after new must be 2-20"
    Just n -> do
      rng <- newStdGen
      let initialGame = evalRand (generateGame n >>= firstClick) rng
      let mineCounts = gameToMineCounts initialGame
      let game = expandNotMinesWithoutNeighbours initialGame mineCounts
      saveGame game
      saveIdrisGame game mineCounts
      putStrLn $ displayGame game mineCounts
      return ()
