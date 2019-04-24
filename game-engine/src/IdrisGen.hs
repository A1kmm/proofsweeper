module IdrisGen (saveIdrisGame) where
import GameModel
import qualified Data.Map as M
import Data.Maybe

isKnown :: (Coord, CellStatus) -> Bool
isKnown (_, CellStatus CellKnown _) = True
isKnown _ = False

genMineFact :: M.Map Coord Int -> (Coord, CellStatus) -> String
genMineFact _ (Coord x y, CellStatus _ CellMine) =
  "  MineAt" ++ (show x) ++ "_" ++ (show y) ++ " : MineFact (MkCoord " ++ (show x) ++ " " ++ (show y) ++ ") IsMine\n"
genMineFact counts (c@(Coord x y), CellStatus _ CellNotMine) =
  "  NoMineAt" ++ (show x) ++ "_" ++ (show y) ++ " : MineFact (MkCoord " ++ (show x) ++ " " ++ (show y) ++ ") (KnownNotMine " ++ (show $ fromMaybe 0 $ M.lookup c counts) ++ ")\n"

genMineFacts :: M.Map Coord Int -> M.Map Coord CellStatus -> String
genMineFacts counts status = concatMap (genMineFact counts) $ filter isKnown $ M.toList status

gameToIdris :: Game -> M.Map Coord Int -> String
gameToIdris (Game { gameGridSize = gridSize, gameStatus = status }) counts =
  "-- Generated source - do not edit!\n\
  \-- This contains the axioms for the current state of the game.\n\
  \module ProofSweeperKnown\n\
  \\n\
  \import ProofSweeperBase\n\
  \\n\
  \%access public export\n\
  \%default total\n\
  \\n\
  \gridSize : Nat\n\
  \gridSize = " ++ show gridSize ++ "\n\
  \\n\
  \mineNeighboursForSize : Coord -> List Coord\n\
  \mineNeighboursForSize = mineNeighbours gridSize\n\
  \\n\
  \data MineFact : Coord -> MineProp -> Type where\n\
  \  KnownNotMineIsNotMine : MineFact c (KnownNotMine _) -> MineFact c IsNotMine\n\
  \  AllMinesAccountedFor :\n\
  \       (c : Coord)\n\
  \    -> (cNonMine : Coord)\n\
  \    -> (prfCIsNotMine : MineFact c (KnownNotMine cnt))\n\
  \    -> (knownMines : List Coord)\n\
  \    -> (prfEnoughKnownMines : distinctCount knownMines = cnt)\n\
  \    -> (prfKnownMinesAreMines : (cNeigh : Coord)\n\
  \           -> elem cNeigh knownMines = True\n\
  \           -> MineFact cNeigh IsMine)\n\
  \    -> (prfKnownMinesAreNeighbours : (cNeigh : Coord)\n\
  \           -> elem cNeigh knownMines = True\n\
  \           -> elem cNeigh (mineNeighboursForSize c) = True)\n\
  \    -> (prfNonMineIsNeighbour : elem cNonMine (mineNeighboursForSize c) = True)\n\
  \    -> (prfNonMineNotInKnownMines : elem cNonMine knownMines = False)\n\
  \    -> MineFact cNonMine IsNotMine\n\
  \  AllNonMinesAccountedFor :\n\
  \       (c : Coord)\n\
  \    -> (cMine : Coord)\n\
  \    -> (prfCIsNotMine : MineFact c (KnownNotMine cnt))\n\
  \    -> (knownNonMines : List Coord)\n\
  \    -> (prfEnoughKnownNonMines : distinctCount knownNonMines + cnt = length (mineNeighboursForSize c))\n\
  \    -> (prfKnownNonMinesAreNotMines : (cNeigh : Coord)\n\
  \           -> elem cNeigh knownNonMines = True\n\
  \           -> MineFact cNeigh IsNotMine)\n\
  \    -> (prfKnownNonMinesAreNeighbours : (cNeigh : Coord)\n\
  \           -> elem cNeigh knownNonMines = True\n\
  \           -> elem cNeigh (mineNeighboursForSize c) = True)\n\
  \    -> (prfNonMineIsNeighbour : elem cMine (mineNeighboursForSize c) = True)\n\
  \    -> (prfMineNotInKnownNonMines : elem cMine knownNonMines = False)\n\
  \    -> MineFact cMine IsMine\n"
    ++ genMineFacts counts status ++ "\n\
  \\n\
  \notMineImpliesNonMine : Not (MineFact c IsMine) -> MineFact c IsNotMine\n\
  \notMineImpliesNonMine v = believe_me v\n\
  \nonMineImpliesNotMine : MineFact c IsNotMine -> Not (MineFact c IsMine)\n\
  \nonMineImpliesNotMine v = believe_me v\n\
  \mineOrNot :\n\
  \       (c : Coord)\n\
  \    -> Either (MineFact c IsMine) (MineFact c IsNotMine)\n\
  \mineOrNot v = believe_me v\n"
  
saveIdrisGame :: Game -> M.Map Coord Int -> IO ()
saveIdrisGame g facts = do
  let idrisFile = gameToIdris g facts
  writeFile "ProofSweeperKnown.idr" idrisFile
