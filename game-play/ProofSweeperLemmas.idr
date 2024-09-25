module ProofSweeperLemmas

import ProofSweeperBase
import ProofSweeperKnown
import Data.Nat

%default total

-- Some lemmas about Nat to support reasoning about Coord
public export
natSAEqSBTrueImpliesAEqBTrue : (a : Nat) -> (b : Nat) ->
                            (S a == S b = True) -> a == b = True
natSAEqSBTrueImpliesAEqBTrue a b prf = prf

public export
natAEqBTrueImpliesEq : (a : Nat) -> (b : Nat) -> (a == b = True) -> a = b
natAEqBTrueImpliesEq Z Z _ = Refl
natAEqBTrueImpliesEq Z (S x) prf = absurd prf
natAEqBTrueImpliesEq (S x) Z prf = absurd prf
natAEqBTrueImpliesEq (S x) (S y) prf =
  eqSucc x y (natAEqBTrueImpliesEq x y (natSAEqSBTrueImpliesAEqBTrue x y prf))

-- Some lemmas about Bool to support reasoning about Coord
public export
aAndBTrueImpliesATrue : (a : Bool) -> (b : Bool) -> (a && b = True) -> a = True
aAndBTrueImpliesATrue True True prf = prf
aAndBTrueImpliesATrue True False prf = absurd prf
aAndBTrueImpliesATrue False _ prf = absurd prf

public export
aAndBTrueImpliesBTrue : (a : Bool) -> (b : Bool) -> (a && b = True) -> b = True
aAndBTrueImpliesBTrue True True prf = prf
aAndBTrueImpliesBTrue True False prf = absurd prf
aAndBTrueImpliesBTrue False _ prf = absurd prf

-- If two Coords test equal, they are equal
public export
eqTestIsEqCoord : (v1 : Coord) -> (v2 : Coord) -> (v1 == v2 = True) -> v1 = v2
eqTestIsEqCoord (MkCoord v1x v1y) (MkCoord v2x v2y) prf =
    rewrite v1y_is_v2y in (rewrite v1x_is_v2x in Refl)
  where
    v1x_is_v2x : (v1x = v2x)
    v1x_is_v2x = natAEqBTrueImpliesEq _ _ $ aAndBTrueImpliesATrue _ _ prf
    v1y_is_v2y : (v1y = v2y)
    v1y_is_v2y = natAEqBTrueImpliesEq _ _ $ aAndBTrueImpliesBTrue _ _ prf

-- If, with a proof you took the branch you did...
public export
ifWithProof : (x : Bool) -> (x = True -> a) -> (x = False -> a) -> a
ifWithProof True ifTrue _ = ifTrue Refl
ifWithProof False _ ifFalse = ifFalse Refl

-- Eliminate an inconsequential head element from a list in a foldl
public export
foldlEliminate1 : {xs : List a}
  -> f init x = init
  -> foldl f init (x::xs) = result
  -> foldl f init xs = result
foldlEliminate1 prfEq prf =
  trans (cong (\x => foldl f x xs) (sym prfEq)) prf

-- If the element is in the list, but not in the head, it's in the tail
public export
elemInTail : Eq a =>
     {v, x : a}
  -> {xs : List a}
  -> v == x = False
  -> elem v (x :: xs) = True
  -> elem v xs = True
elemInTail prfNotEq prfElem =
  foldlEliminate1 {x} {xs} {init = False} prfNotEq prfElem

-- If we have a list with proofs, we can produce a proof for any single element in the list
public export
trueForAllListElems : Eq k =>
     {p : k -> Type}
  -> (prfEq : (v1 : k) -> (v2 : k) -> (v1 == v2 = True) -> v1 = v2)
  -> (xs : List (DPair k p))
  -> (v : k)
  -> elem v (map DPair.fst xs) = True
  -> p v
trueForAllListElems prfEq [] v prfElem = absurd prfElem
trueForAllListElems prfEq ((x ** p)::xs) v prfElem = ifWithProof (v == x)
  (\prfEqTrue =>
    rewrite prfEq v x prfEqTrue in p
  )
  (\prfEqFalse =>
    trueForAllListElems prfEq xs v (elemInTail prfEqFalse prfElem)
  )

public export
notNonMineImpliesMine : {c : Coord} -> Not (MineFact c IsNotMine) -> MineFact c IsMine
notNonMineImpliesMine {c} prfNotNonMine = case mineOrNot c of
  Left prfCIsMine => prfCIsMine
  Right prfNonMine => absurd (prfNotNonMine prfNonMine)

public export
mineImpliesNotNonMine : MineFact c IsMine -> Not (MineFact c IsNotMine)
mineImpliesNotNonMine prfMine prfNonMine = nonMineImpliesNotMine prfNonMine prfMine
