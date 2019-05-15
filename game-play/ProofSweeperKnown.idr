-- Generated source - do not edit!
-- This contains the axioms for the current state of the game.
module ProofSweeperKnown

import ProofSweeperBase

%access public export
%default total

gridSize : Nat
gridSize = 8

mineNeighboursForSize : Coord -> List Coord
mineNeighboursForSize = mineNeighbours gridSize

data MineFact : Coord -> MineProp -> Type where
  KnownNotMineIsNotMine : MineFact c (KnownNotMine _) -> MineFact c IsNotMine
  AllMinesAccountedFor :
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
    -> (prfNonMineIsNeighbour : elem cMine (mineNeighboursForSize c) = True)
    -> (prfMineNotInKnownNonMines : elem cMine knownNonMines = False)
    -> MineFact cMine IsMine
  NotMineImpliesNonMine :
       Not (MineFact c IsMine)
    -> MineFact c IsNotMine
  NoMineAt0_0 : MineFact (MkCoord 0 0) (KnownNotMine 0)
  NoMineAt0_1 : MineFact (MkCoord 0 1) (KnownNotMine 1)
  NoMineAt1_0 : MineFact (MkCoord 1 0) (KnownNotMine 0)
  NoMineAt1_1 : MineFact (MkCoord 1 1) (KnownNotMine 2)
  NoMineAt2_0 : MineFact (MkCoord 2 0) (KnownNotMine 1)
  NoMineAt2_1 : MineFact (MkCoord 2 1) (KnownNotMine 3)


nonMineImpliesNotMine : MineFact c IsNotMine -> Not (MineFact c IsMine)
nonMineImpliesNotMine v = believe_me v
mineOrNot :
       (c : Coord)
    -> Either (MineFact c IsMine) (MineFact c IsNotMine)
mineOrNot v = believe_me v
