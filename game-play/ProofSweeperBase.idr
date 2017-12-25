-- Base library for proof sweeper. Don't change this during normal game-play, as it
-- contains definitions central to the rules of the game.

module ProofSweeperBase

%access public export
%default total

data Coord = MkCoord Nat Nat

implementation Eq Coord where
  (MkCoord x1 y1) == (MkCoord x2 y2) = x1 == x2 && y1 == y2

data MineProp : Type where
  IsMine : MineProp
  IsNotMine : MineProp
  KnownNotMine : Nat -> MineProp

neighbouringOrdinates : Nat -> Nat -> List Nat
neighbouringOrdinates _ Z = 0 :: 1 :: Nil
neighbouringOrdinates gridSize (S n) with (n + 2 >= gridSize)
  neighbouringOrdinates _ (S n) | True  = n :: (S n) :: Nil
  neighbouringOrdinates _ (S n) | False  = n :: (S n) :: S (S n) :: Nil
  
allPairs : (a -> b -> c) -> List a -> List b -> List c
allPairs _ [] y = []
allPairs f (h::t) y = zipWith f (replicate (length y) h) y ++ allPairs f t y

private allPairs_test1 : (allPairs (\x, y => (x, y)) [1,2] [3,4]) = [(1,3),(1,4),(2,3),(2,4)]
allPairs_test1 = Refl

mineNeighbours : Nat -> Coord -> List Coord
mineNeighbours gridSize c@(MkCoord x y) = filter (\x => x /= c) $ allPairs MkCoord (neighbouringOrdinates gridSize x) (neighbouringOrdinates gridSize y)

distinctCount : Eq x => List x -> Nat
distinctCount l = length (nub l)

private distinctCount_test1 : (distinctCount [1,2,3,2,3,5,2,7] = 5)
distinctCount_test1 = Refl
