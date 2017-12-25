module ProofSweeperKnown

import ProofSweeperBase

%access public export
%default total

gridSize : Nat
gridSize = 3

mineNeighboursForSize : Coord -> List Coord
mineNeighboursForSize = mineNeighbours gridSize

data MineFact : Coord -> MineProp -> Type where
  KnownNotMineIsNotMine : MineFact c (KnownNotMine _) -> MineFact c IsNotMine
  AllMinesAcountedFor :
       (c : Coord)
    -> (cNonMine : Coord)
    -> (prfCIsNotMine : MineFact c (KnownNotMine cnt))
    -> (knownMines : List Coord)
    -> (prfEnoughKnownMines : distinctCount knownMines = cnt)
    -> (prfKnownMinesAreMines : (cNeigh : Coord)
           -> elem cNeigh knownMines = True
           -> MineFact cNeigh IsMine)
    -> (prfKnownMinesAreNeighbours : (cNeigh : Coord)
           -> elem cNeigh knownMines = True
           -> elem cNeigh (mineNeighboursForSize c) = True)
    -> (prfNonMineIsNeighbour : elem cNonMine (mineNeighboursForSize c) = True)
    -> (prfNonMineNotInKnownMines : elem cNonMine knownMines = False)
    -> MineFact cNonMine IsNotMine
  AllNonMinesAccountedFor :
       (c : Coord)
    -> (cMine : Coord)
    -> (prfCIsNotMine : MineFact c (KnownNotMine cnt))
    -> (knownNonMines : List Coord)
    -> (prfEnoughKnownNonMines : distinctCount knownNonMines + cnt = length (mineNeighboursForSize c))
    -> (prfKnownNonMinesAreNotMines : (cNeigh : Coord)
           -> elem cNeigh knownNonMines = True
           -> MineFact cNeigh IsNotMine)
    -> (prfKnownNonMinesAreNeighbours : (cNeigh : Coord)
           -> elem cNeigh knownNonMines = True
           -> elem cNeigh (mineNeighboursForSize c) = True)
    -> (prfNonMineIsNeighbour : elem cNonMine (mineNeighboursForSize c) = True)
    -> (prfMineNotInKnownNonMines : elem cMine knownNonMines = False)
    -> MineFact cNonMine IsMine
