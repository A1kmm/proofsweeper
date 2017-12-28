module ProofSweeperLemmas

import ProofSweeperBase

%default total
%access public export

-- Some lemmas about Nat to support reasoning about Coord
natSAEqSBTrueImpliesAEqBTrue : (a : Nat) -> (b : Nat) ->
                            (S a == S b = True) -> a == b = True
natSAEqSBTrueImpliesAEqBTrue a b prf = prf

natAEqBTrueImpliesEq : (a : Nat) -> (b : Nat) -> (a == b = True) -> a = b
natAEqBTrueImpliesEq Z Z _ = Refl
natAEqBTrueImpliesEq Z (S x) prf = absurd prf
natAEqBTrueImpliesEq (S x) Z prf = absurd prf
natAEqBTrueImpliesEq (S x) (S y) prf =
  eqSucc x y (natAEqBTrueImpliesEq x y (natSAEqSBTrueImpliesAEqBTrue x y prf))

-- Some lemmas about Bool to support reasoning about Coord
aAndBTrueImpliesATrue : (a : Bool) -> (b : Bool) -> (a && b = True) -> a = True
aAndBTrueImpliesATrue True True prf = prf
aAndBTrueImpliesATrue True False prf = absurd prf
aAndBTrueImpliesATrue False _ prf = absurd prf

aAndBTrueImpliesBTrue : (a : Bool) -> (b : Bool) -> (a && b = True) -> b = True
aAndBTrueImpliesBTrue True True prf = prf
aAndBTrueImpliesBTrue True False prf = absurd prf
aAndBTrueImpliesBTrue False _ prf = absurd prf

-- If two Coords test equal, they are equal
eqTestIsEqCoord : (v1 : Coord) -> (v2 : Coord) -> (v1 == v2 = True) -> v1 = v2
eqTestIsEqCoord (MkCoord v1x v1y) (MkCoord v2x v2y) prf =
    rewrite v1y_is_v2y in (rewrite v1x_is_v2x in Refl)
  where
    v1x_is_v2x : (v1x = v2x)
    v1x_is_v2x = natAEqBTrueImpliesEq _ _ $ aAndBTrueImpliesATrue _ _ prf
    v1y_is_v2y : (v1y = v2y)
    v1y_is_v2y = natAEqBTrueImpliesEq _ _ $ aAndBTrueImpliesBTrue _ _ prf

-- If, with a proof you took the branch you did...
ifWithProof : (x : Bool) -> (x = True -> a) -> (x = False -> a) -> a
ifWithProof True ifTrue _ = ifTrue Refl
ifWithProof False _ ifFalse = ifFalse Refl

-- For some proposition p, and assuming testing equal means being equal,
-- and for some element el1, if p holds for el1, then p holds for all elements
-- in a list containing only el1.
trueForAllListElems1 : Eq k =>
    (p : k -> Type)
 -> (prfEq : (v1 : k) -> (v2 : k) -> (v1 == v2 = True) -> v1 = v2)
 -> (el1 : k) -> p el1
 -> (v : k) -> (elem v [el1] = True) -> p v
trueForAllListElems1 p prfEq el1 fact1 v prfElem =
  ifWithProof (v == el1)
    (\prfEqTrue : (v == el1 = True) =>
        (rewrite (prfEq v el1 prfEqTrue) in fact1)
    )
    (\prfEqFalse : (v == el1 = False) =>
      absurd (replace {P = \x => (if x then True else False) = True} prfEqFalse prfElem)
    )

-- The same, if you also prove p holds for el2, and the list has only el1 and el2.
trueForAllListElems2 : Eq k =>
    (p : k -> Type)
 -> (prfEq : (v1 : k) -> (v2 : k) -> (v1 == v2 = True) -> v1 = v2)
 -> (el1 : k) -> (el2 : k)
 -> p el1 -> p el2
 -> (v : k) -> (elem v [el1, el2] = True) -> p v
trueForAllListElems2 p prfEq el1 el2 fact1 fact2 v prfElem =
  ifWithProof (v == el1)
    (\prfEqTrue : (v == el1 = True) =>
        (rewrite (prfEq v el1 prfEqTrue) in fact1)
    )
    (\prfEqFalse : (v == el1 = False) =>
      trueForAllListElems1 p prfEq el2 fact2 v (
        replace {P = \x => (if x then True else
                              if v == el2 then True else
                                False) = True} prfEqFalse prfElem
      )
    )

trueForAllListElems3 : Eq k =>
    (p : k -> Type)
 -> (prfEq : (v1 : k) -> (v2 : k) -> (v1 == v2 = True) -> v1 = v2)
 -> (el1 : k) -> (el2 : k) -> (el3 : k)
 -> p el1 -> p el2 -> p el3
 -> (v : k) -> (elem v [el1, el2, el3] = True) -> p v
trueForAllListElems3 p prfEq el1 el2 el3 fact1 fact2 fact3 v prfElem =
  ifWithProof (v == el1)
    (\prfEqTrue : (v == el1 = True) =>
        (rewrite (prfEq v el1 prfEqTrue) in fact1)
    )
    (\prfEqFalse : (v == el1 = False) =>
      trueForAllListElems2 p prfEq el2 el3 fact2 fact3 v (
        replace {P = \x => (if x then True else
                              if v == el2 then True else
                              if v == el3 then True else
                                False) = True} prfEqFalse prfElem
      )
    )

trueForAllListElems4 : Eq k =>
    (p : k -> Type)
 -> (prfEq : (v1 : k) -> (v2 : k) -> (v1 == v2 = True) -> v1 = v2)
 -> (el1 : k) -> (el2 : k) -> (el3 : k) -> (el4 : k)
 -> p el1 -> p el2 -> p el3 -> p el4
 -> (v : k) -> (elem v [el1, el2, el3, el4] = True) -> p v
trueForAllListElems4 p prfEq el1 el2 el3 el4 fact1 fact2 fact3 fact4 v prfElem =
  ifWithProof (v == el1)
    (\prfEqTrue : (v == el1 = True) =>
        (rewrite (prfEq v el1 prfEqTrue) in fact1)
    )
    (\prfEqFalse : (v == el1 = False) =>
      trueForAllListElems3 p prfEq el2 el3 el4 fact2 fact3 fact4 v (
        replace {P = \x => (if x then True else
                              if v == el2 then True else
                              if v == el3 then True else
                              if v == el4 then True else
                                False) = True} prfEqFalse prfElem
      )
    )

trueForAllListElems5 : Eq k =>
    (p : k -> Type)
 -> (prfEq : (v1 : k) -> (v2 : k) -> (v1 == v2 = True) -> v1 = v2)
 -> (el1 : k) -> (el2 : k) -> (el3 : k) -> (el4 : k) -> (el5 : k)
 -> p el1 -> p el2 -> p el3 -> p el4 -> p el5
 -> (v : k) -> (elem v [el1, el2, el3, el4, el5] = True) -> p v
trueForAllListElems5 p prfEq el1 el2 el3 el4 el5 fact1 fact2 fact3 fact4 fact5 v prfElem =
  ifWithProof (v == el1)
    (\prfEqTrue : (v == el1 = True) =>
        (rewrite (prfEq v el1 prfEqTrue) in fact1)
    )
    (\prfEqFalse : (v == el1 = False) =>
      trueForAllListElems4 p prfEq el2 el3 el4 el5 fact2 fact3 fact4 fact5 v (
        replace {P = \x => (if x then True else
                              if v == el2 then True else
                              if v == el3 then True else
                              if v == el4 then True else
                              if v == el5 then True else
                                False) = True} prfEqFalse prfElem
      )
    )

trueForAllListElems6 : Eq k =>
    (p : k -> Type)
 -> (prfEq : (v1 : k) -> (v2 : k) -> (v1 == v2 = True) -> v1 = v2)
 -> (el1 : k) -> (el2 : k) -> (el3 : k) -> (el4 : k) -> (el5 : k) -> (el6 : k)
 -> p el1 -> p el2 -> p el3 -> p el4 -> p el5 -> p el6
 -> (v : k) -> (elem v [el1, el2, el3, el4, el5, el6] = True) -> p v
trueForAllListElems6 p prfEq el1 el2 el3 el4 el5 el6 fact1 fact2 fact3 fact4 fact5 fact6 v prfElem =
  ifWithProof (v == el1)
    (\prfEqTrue : (v == el1 = True) =>
        (rewrite (prfEq v el1 prfEqTrue) in fact1)
    )
    (\prfEqFalse : (v == el1 = False) =>
      trueForAllListElems5 p prfEq el2 el3 el4 el5 el6 fact2 fact3 fact4 fact5 fact6 v (
        replace {P = \x => (if x then True else
                              if v == el2 then True else
                              if v == el3 then True else
                              if v == el4 then True else
                              if v == el5 then True else
                              if v == el6 then True else
                                False) = True} prfEqFalse prfElem
      )
    )

trueForAllListElems7 : Eq k =>
    (p : k -> Type)
 -> (prfEq : (v1 : k) -> (v2 : k) -> (v1 == v2 = True) -> v1 = v2)
 -> (el1 : k) -> (el2 : k) -> (el3 : k) -> (el4 : k) -> (el5 : k) -> (el6 : k)
 -> (el7 : k)
 -> p el1 -> p el2 -> p el3 -> p el4 -> p el5 -> p el6 -> p el7
 -> (v : k) -> (elem v [el1, el2, el3, el4, el5, el6, el7] = True) -> p v
trueForAllListElems7 p prfEq el1 el2 el3 el4 el5 el6 el7 fact1 fact2 fact3 fact4 fact5 fact6 fact7 v prfElem =
  ifWithProof (v == el1)
    (\prfEqTrue : (v == el1 = True) =>
        (rewrite (prfEq v el1 prfEqTrue) in fact1)
    )
    (\prfEqFalse : (v == el1 = False) =>
      trueForAllListElems6 p prfEq el2 el3 el4 el5 el6 el7 fact2 fact3 fact4 fact5 fact6 fact7 v (
        replace {P = \x => (if x then True else
                              if v == el2 then True else
                              if v == el3 then True else
                              if v == el4 then True else
                              if v == el5 then True else
                              if v == el6 then True else
                              if v == el7 then True else
                                False) = True} prfEqFalse prfElem
      )
    )

trueForAllListElems8 : Eq k =>
    (p : k -> Type)
 -> (prfEq : (v1 : k) -> (v2 : k) -> (v1 == v2 = True) -> v1 = v2)
 -> (el1 : k) -> (el2 : k) -> (el3 : k) -> (el4 : k) -> (el5 : k) -> (el6 : k)
 -> (el7 : k) -> (el8 : k)
 -> p el1 -> p el2 -> p el3 -> p el4 -> p el5 -> p el6 -> p el7 -> p el8
 -> (v : k) -> (elem v [el1, el2, el3, el4, el5, el6, el7, el8] = True) -> p v
trueForAllListElems8 p prfEq el1 el2 el3 el4 el5 el6 el7 el8 fact1 fact2 fact3 fact4 fact5 fact6 fact7 fact8 v prfElem =
  ifWithProof (v == el1)
    (\prfEqTrue : (v == el1 = True) =>
        (rewrite (prfEq v el1 prfEqTrue) in fact1)
    )
    (\prfEqFalse : (v == el1 = False) =>
      trueForAllListElems7 p prfEq el2 el3 el4 el5 el6 el7 el8 fact2 fact3 fact4 fact5 fact6 fact7 fact8 v (
        replace {P = \x => (if x then True else
                              if v == el2 then True else
                              if v == el3 then True else
                              if v == el4 then True else
                              if v == el5 then True else
                              if v == el6 then True else
                              if v == el7 then True else
                              if v == el8 then True else
                                False) = True} prfEqFalse prfElem
      )
    )
