module GameMove (doMoveCommand) where
import GameModel
import GameUtils
import GameIO
import GameDisplay
import IdrisGen
import System.Process
import qualified Data.Map as M

processMove :: Coord -> String -> String -> IO ()
processMove c@(Coord x y) varPrefix expectedType = do
  let factCheck = "module CheckMove\n\
                 \  import ProofSweeperBase\n\
                 \  import ProofSweeperKnown\n\
                 \  import ProofSweeperPlay\n\
                 \  total\n\
                 \  checkMove : MineFact (MkCoord " ++ (show x) ++ " " ++ (show y)
                      ++ ") " ++ expectedType ++ "\n\
                 \  checkMove = " ++ varPrefix ++ "_" ++ (show x) ++ "_"
                      ++ (show y) ++ "\n"
  writeFile "CheckMove.idr" factCheck
  callProcess "idris" ["--check", "CheckMove.idr"]
  Just game <- loadGame
  case M.lookup (Coord x y) (gameStatus game) of
    Just statusNow -> do
      let newStatus = statusNow { cellVisibility = CellKnown }
      let counts = gameToMineCounts game
      let game' = expandNotMinesWithoutNeighbours
                    (game { gameStatus = M.insert c newStatus (gameStatus game) })
                    counts
      saveGame game'
      saveIdrisGame game' counts
      putStrLn $ displayGame game' counts
    Nothing -> putStrLn "Proof of something that is not true - did you abuse believe_me / assert_total etc...?"
  
doMoveCommand :: String -> String -> String -> IO ()
doMoveCommand commandName xStr yStr =
    case (parseNumber xStr, parseNumber yStr) of
      (Just x, Just y) ->
        case commandName of
          "mine" -> processMove (Coord x y) "mineAt" "IsMine"
          "notmine" -> processMove (Coord x y) "noMineAt" "IsNotMine"
          _ -> putStrLn "First argument to move must be mine or notmine"
      _ -> putStrLn "Expected numbers as second and third argument to move"
