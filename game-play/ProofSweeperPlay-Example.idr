module ProofSweeperPlay
import ProofSweeperKnown
import ProofSweeperBase
import ProofSweeperLemmas

%access public export

mineAt_14_9 : MineFact (MkCoord 14 9) IsMine
mineAt_14_9 =
  AllNonMinesAccountedFor 
    -- Known non-mine at 14,10 is a '1'...
    (MkCoord 14 10)
    -- We want to prove that 14,9 is a mine
    (MkCoord 14 9)
    -- We need to prove 14,10 is in fact a non-mine
    NoMineAt14_10
    -- As a cell on the wall, 14,10 has 5 neighbours, so we need to list 4
    -- neighbours and prove the aren't mines.
    [MkCoord 13 9, MkCoord 13 10,
     MkCoord 13 11, MkCoord 14 11]
    -- prfEnoughKnownNonMines - Idris can prove that the above list is big enough
    --  just from Refl
    Refl
    -- prfKnownNonMinesAreNotMines - We need to prove that each of the 4 are
    -- not actually mines...
    (trueForAllListElems4
      (\x => MineFact x IsNotMine) eqTestIsEqCoord
      (MkCoord 13 9) (MkCoord 13 10)
      (MkCoord 13 11) (MkCoord 14 11)
      (KnownNotMineIsNotMine NoMineAt13_9)
      (KnownNotMineIsNotMine NoMineAt13_10)
      (KnownNotMineIsNotMine NoMineAt13_11)
      (KnownNotMineIsNotMine NoMineAt14_11)
    )
    -- prfKnownNonMinesAreNeighbours - We need to break out into individual
    --  proofs, and then Idris can simplify from Refl
    (trueForAllListElems4
      (\x => elem x (mineNeighboursForSize (MkCoord 14 10)) = True) eqTestIsEqCoord
      (MkCoord 13 9) (MkCoord 13 10) (MkCoord 13 11) (MkCoord 14 11)
      Refl Refl Refl Refl
    )
    -- prfNonMineIsNeighbour - that 14,9 neighbours 14,10 follows automatically by
    --   simplification to Refl
    Refl
    -- prfMineNotInKnownNonMines - follows automatically by simplification to Refl
    Refl

{- A more complex example of a formal proof:
   There is a pattern like:
     2111|
     2111|
      xyz|
      
   Here is a hand-wavy proof that x isn't a mine:
   
   The x cannot be a mine, because if it was, then y couldn't also be a mine,
   due to the '1'. If y isn't a mine, then z must be a mine, so the 1 on the end
   is touching at least one mine. But x and z can't both be mines, since the
   1 above the y can only be touching one mine.
   
   Now we convert that to a formal proof:
 -}
 
XCoord : Coord
XCoord = MkCoord 12 13
YCoord : Coord
YCoord = MkCoord 13 13
ZCoord : Coord
ZCoord = MkCoord 14 13

contradictionXMine : MineFact XCoord IsMine -> Void
contradictionXMine hXMine =
  let
    aboveYCoord : Coord = MkCoord 13 12
    aboveZCoord : Coord = MkCoord 14 12
    twoAboveYCoord : Coord = MkCoord 13 11
    twoAboveZCoord : Coord = MkCoord 14 11
    yIsNotMine : MineFact YCoord IsNotMine =
      AllMinesAccountedFor
        aboveYCoord YCoord NoMineAt13_12 [XCoord] Refl
        -- We need to show that XCoord is a mine...
        (trueForAllListElems1 (\x => MineFact x IsMine) eqTestIsEqCoord
           XCoord hXMine)
        -- We need to show that XCoord is a neighbour of aboveYCoord...
        (trueForAllListElems1 (\x => elem x (mineNeighboursForSize aboveYCoord) = True)
           eqTestIsEqCoord
           XCoord Refl)
        -- And that YCoord is also a neighbour...
        Refl
        -- And that YCoord is not in [XCoord]
        Refl
    zIsNotMine : MineFact ZCoord IsNotMine =
      AllMinesAccountedFor
        aboveYCoord ZCoord NoMineAt13_12 [XCoord] Refl
        -- We need to show that XCoord is a mine...
        (trueForAllListElems1 (\x => MineFact x IsMine) eqTestIsEqCoord
           XCoord hXMine)
        -- We need to show that XCoord is a neighbour of aboveYCoord...
        (trueForAllListElems1 (\x => elem x (mineNeighboursForSize aboveYCoord) = True)
           eqTestIsEqCoord
           XCoord Refl)
        -- And that ZCoord is also a neighbour...
        Refl
        -- And that ZCoord is not in [XCoord]
        Refl
    -- Now we construct a contradictory fact resulting from yIsNotMine...
    zIsMine : MineFact ZCoord IsMine =
      AllNonMinesAccountedFor
        aboveZCoord ZCoord NoMineAt14_12
        [twoAboveYCoord, twoAboveZCoord, aboveYCoord, YCoord] Refl
        -- Each of the surrounding Coords are not mines...
        (trueForAllListElems4 (\x => MineFact x IsNotMine) eqTestIsEqCoord
          twoAboveYCoord twoAboveZCoord aboveYCoord YCoord
          (KnownNotMineIsNotMine NoMineAt13_11)
          (KnownNotMineIsNotMine NoMineAt14_11)
          (KnownNotMineIsNotMine NoMineAt13_12)
          yIsNotMine)
        -- Surrounding Coords are neighbours of aboveZCoord
        (trueForAllListElems4 (\x => elem x (mineNeighboursForSize aboveZCoord) = True)
          eqTestIsEqCoord
          twoAboveYCoord twoAboveZCoord aboveYCoord YCoord
          Refl           Refl           Refl        Refl
        )
        -- And that ZCoord is also a neighbour...
        Refl
        -- And that ZCoord is not in [twoAboveYCoord, twoAboveZCoord, aboveYCoord, YCoord]
        Refl
  in
    (nonMineImpliesNotMine zIsNotMine) zIsMine

noMineAt_12_13 : MineFact XCoord IsNotMine
noMineAt_12_13 = notMineImpliesNonMine contradictionXMine
