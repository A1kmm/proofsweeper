import System.Environment (getArgs)
import GameCreation (doNewCommand)
import GameMove (doMoveCommand)
import GameModel
import GameIO
import GameDisplay
import GameUtils
import IdrisGen
import qualified Data.Map as M

showUsage :: IO ()
showUsage = putStrLn "\
  \Usage: ProofSweeperEngine <subCommand>\n\
  \  Sub-commands:\n\
  \    new <n>       Create a new n*n game, replacing current game\n\
  \    print         Print current status of game\n\
  \    hint <x> <y>  Get a hint\n\
  \    move <mine|notmine> <x> <y>\n\
  \         Check proof that (x,y) is or isn't a mine. You must have\n\
  \         defined mineAt_x_y or noMineAt_x_y in ProofSweeperPlay.idr\n\
  \         with the type MineFact (MkCoord x y) IsMine (or IsNotMine).\n"

doPrintCommand :: IO ()
doPrintCommand = do
  Just game <- loadGame
  let counts = gameToMineCounts game
  putStrLn $ displayGame game counts

doHintCommand :: String -> String -> IO ()
doHintCommand xStr yStr = do
    Just game <- loadGame
    case (parseNumber xStr, parseNumber yStr) of
      (Just x, Just y) ->
        case M.lookup (Coord x y) (gameStatus game) of
          Nothing -> putStrLn "No such coordinate found"
          Just curStatus -> do
            let counts = gameToMineCounts game
            let game' = expandNotMinesWithoutNeighbours
                          (applyHint game (Coord x y) curStatus) counts
            saveGame game'
            saveIdrisGame game' counts
            putStrLn $ displayGame game' counts
      (_, _) -> putStrLn "Expected two numbers after hint"
  where
    applyHint game coord cellStatus =
      game { gameStatus = M.insert coord (cellStatus { cellVisibility = CellKnown }) (gameStatus game) }

main :: IO ()
main = do
  args <- getArgs
  case args of
    "new":nStr:_-> doNewCommand nStr
    "print":_ -> doPrintCommand
    "hint":x:y:_ -> doHintCommand x y
    "move":moveType:x:y:_ -> doMoveCommand moveType x y
    _ -> showUsage
