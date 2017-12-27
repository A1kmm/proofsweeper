module GameIO (saveGame, loadGame) where

import qualified Data.Map as M
import qualified Text.Parsec as P
import qualified Text.Parsec.Language as P
import qualified Text.Parsec.Token as P
import Control.DeepSeq
import Control.Applicative
import GameModel
import GameUtils

gameToString :: Game -> String
gameToString (Game gridSize status) =
  (show gridSize) ++ "," ++ [cellStatusToChar (M.lookup (Coord x y) status) |
                          x <- [0..(gridSize - 1)], y <- [0..(gridSize - 1)]]
  where
    cellStatusToChar Nothing = 'x'
    cellStatusToChar (Just (CellStatus CellKnown CellMine)) = 'M'
    cellStatusToChar (Just (CellStatus CellKnown CellNotMine)) = 'N'
    cellStatusToChar (Just (CellStatus CellUnknown CellMine)) = 'm'
    cellStatusToChar (Just (CellStatus CellUnknown CellNotMine)) = 'n'
    
saveGame :: Game -> IO ()
saveGame g = writeFile "game.data" (gameToString g)

parseCellStatus :: P.Parsec String u CellStatus
parseCellStatus =
      (P.char 'M' *> pure (CellStatus CellKnown CellMine))
  <|> (P.char 'N' *> pure (CellStatus CellKnown CellNotMine))
  <|> (P.char 'm' *> pure (CellStatus CellUnknown CellMine))
  <|> (P.char 'n' *> pure (CellStatus CellUnknown CellNotMine))

parseGame :: P.Parsec String u Game
parseGame = do
  size <- fromIntegral <$> P.decimal P.haskell
  _ <- P.char ','
  cellStatuses <- P.count (size * size) parseCellStatus
  let statuses = M.fromList $ zip [Coord x y | x <- [0..(size - 1)],
                               y <- [0..(size - 1)]] cellStatuses
  return $ Game size statuses

loadGame :: IO (Maybe Game)
loadGame = do
  gameData <- readFile "game.data"
  gameData `deepseq` (return . eitherToJust $ P.parse parseGame "" gameData)
