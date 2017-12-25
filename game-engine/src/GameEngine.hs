import System.Environment (getArgs)
import qualified Data.Map as M
import System.Random
import qualified Text.Parsec as P
import qualified Text.Parsec.Language as P
import qualified Text.Parsec.Token as P

data Coord = Coord Int Int deriving (Eq, Ord)
data CellMineStatus = CellMine | CellNotMine
data CellVisibility = CellKnown | CellUnknown
data CellStatus = CellStatus CellVisibility CellMineStatus

data Game = Game {
    gridSize :: Int
  , gameStatus :: M.Map Coord CellStatus
                 }

eitherToJust :: Either a b -> Maybe b
eitherToJust = either (const Nothing) Just

parseNumber :: String -> Maybe Int
parseNumber nStr = fmap fromIntegral $
  eitherToJust $ P.parse (P.decimal P.haskell) "" nStr

generateGame :: Int -> StdGen -> Game
generateGame gridSize rng =
    placeMines nMines (Game { gridSize = gridSize, gameStatus = blankGrid })
  where
    blankGrid = M.fromList [(Coord x y, CellStatus CellUnknown CellNotMine) |
                            x <- [0..(gridSize - 1)], y <- [0..(gridSize - 1)]]

doNewCommand :: String -> IO ()
doNewCommand nStr =
  case parseNumber nStr of
    Nothing -> putStrLn "Expected number after new"
    Just n | n <= 1 || n > 20 -> putStrLn "Number after new must be 2-20"
    Just n -> do
      rng <- newStdGen
      let game = generateGame n rng
      return ()

showUsage :: IO ()
showUsage = putStrLn "\
  \Usage: GameEngine <subCommand>\n\
  \  Sub-commands:\n\
  \    new <n>       Create a new n*n game, replacing current game"

main :: IO ()
main = do
  args <- getArgs
  case args of
    "new":nStr:_-> doNewCommand nStr
    _ -> showUsage
