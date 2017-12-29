# About ProofSweeper

ProofSweeper is a variant of the classic Minesweeper game in which, to advance in the
game, you must formally prove that a cell is or is not a mine using [Idris](https://www.idris-lang.org/). The game is designed as a fun way to improve your ability to write
formal proofs in Idris.

It is played on a square grid of cells (2x2 up to 20x20, selected when you create a
new game). The game is played by using a command-line utility called the ProofSweeperEngine
to control game flow, and by writing proofs in your favourite text editor.

The ProofSweeperEngine is responsible for generating the axioms (things known to be true,
e.g. that a certain cell is not a mine but is touching 2 mines) resulting from the
current state of the game. It also produces a console visualisation of the state of
play to help you plan what formal proofs you want to make. When you have a formal
proof, the ProofSweeperEngine is also responsible for invoking Idris to validate your proof
is valid, and updating everything for the new state.

# Getting started

* Before you can use ProofSweeper, you need to have Idris installed and on the PATH. ProofSweeper has been tested with Idris-1.1.1, but may work with other versions of Idris. Follow the [Idris installation instructions](https://github.com/idris-lang/Idris-dev/wiki/Installation-Instructions), and check that invoking `idris` from the command line works before continuing.
* You also need Haskell Stack installed. Refer to the [Stack installation instructions](https://docs.haskellstack.org/en/stable/README/) to install Stack. Check that you can run `stack` on the command-line before continuing.
* If you haven't already, clone this ProofSweeper repository with `git clone https://github.com/A1kmm/proofsweeper.git`, and change into the proofsweeper directory.
* Change into the game-engine subdirectory, and run `stack install`. Check that you can run `ProofSweeperEngine` from the command-line.
* To play the game, you should always change into the game-play subdirectory of the proofsweeper repository.
* Decide how big of a board you want to play on. 15 is a reasonable starting point if you want a long game. From the game-play subdirectory, run `ProofSweeperEngine new 15` (or whatever board size you want).
* The game will print out an ASCII art representation of your game, and will also
  generate some files to support game play.

# How to play

Typical game-play flow is as follows:

* Change into the game-play subdirectory.
* Run the ProofSweeperEngine with the new command to set up a game.
* Create ProofSweeperPlay.idr (look at ProofSweeperPlay-Example.idr as an example of
  how to create the file).
* Write a proof (see below for more information) that an unknown cell is either a
  mine, or is not a mine. The proof must be named `mineAt_x_y` or `noMineAt_x_y`
  (where `x` and `y` are replaced with the numbers of the coordinate the proof
  relates to). You may want to use an Idris environment to verify your proof as you
  go.
* To build a proof, you need to derive `MineFact (MkCoord x y) IsMine` or `MineFact (MkCoord x y) IsNotMine` from the axioms.
* Once you have a valid proof with no holes (metavariables) unsolved, run `ProofSweeperEngine move mine x y` or `ProofSweeperEngine move notmine x y` to check your proof. If it is accepted, the axioms will be updated (which, in the case of a non-mine, will likely expose new information in the count of adjacent mines), and a new ASCII art representation of the board will be printed.
* Repeat the above until the mine / non mine status of all cells are known!
* If you get stuck (sometimes there is not enough information visible to prove anything), you can use the `ProofSweeperEngine hint x y` command to expose any unknown cell, which might provide enough information to continue proving.
* If you want to see a visualisation of the game state again, you can use the
  `ProofSweeperEngine print` command.

# Rules of the game

## General rules
* The game is played on a square grid (2x2 to 20x20) made up of cells.
* Every cell is either a mine or a non-mine. About 25% of cells are mines, but the
  total number of mines is a secret until all cells are revealed, so you can't rely
  on this.
* At any one time, a cell is either known (which means you can directly observe
  whether or not it is a mine) or unknown (which means that you cannot directly
  observe whether it is a mine - but you may be able to infer whether it is).
* If a known cell is not a mine, you can also observe how many neighbouring cells are
  mines (even if they are unknown). A cell has up to 8 neighbours: the directly
  adjacent cells above, below, left, right, diagonally above and left, above and right, below and left, and below and right. Some cells have fewer neighbours because they are on an edge or corner (edges do not wrap around).
* A cell which is known stays known for the rest of the game.
* You win when all cells are known.

## ProofSweeper rules

* A cell which is unknown becomes known if you prove either that it is a mine, or
  that it isn't a mine.
* During game play, you should not use assert_total, believe_me, or other similar
  assertion functions, as these could undermine the soundness of the game.
* You should not modify ProofSweeperKnown.idr or ProofSweeperBase.idr to help you
  win the game, as that would be cheating.
* You can use and modify the lemmas in ProofSweeperLemmas.idr, or write your own
  lemmas and utilities to help you play the game (either in ProofSweeperPlay.idr, or
  your own file).
* Don't look at or modify `game.data` - it contains the full state of the game,
  including 'unknown' cells.
* You may want to keep track of how many times you use the `hint` command and compete
  with others to see who uses it the least to complete the board!

# Constructing proofs

## Supporting objects

* Have a look in ProofSweeperBase.idr. Coord represents the coordinate of a cell
  (e.g. MkCoord 0 0 represents the top left corner of the grid).
* MineProp (also in ProofSweeperBase) represents the possible true state of a cell
  (whether known or not). IsMine and IsNotMine represent a cell is, or is not a mine.
  KnownNotMine is more specialised, and represents both that a cell is not a mine,
  and how many neighbouring mines it has.
* Note that you can construct any MineProp you like, but it doesn't mean that it is
  true.
* Now have a look in ProofSweeperKnown.idr (this file is generated and updated as
  game play progresses, so you must have a game in progress to look at the file).
  Notice gridSize, which defines the size of the grid.

## Mine facts

* MineFact (also defined in the generated file ProofSweeperKnown.idr) is the key data
  type (GADT) for representing a proof of whether or not a given cell is a mine.
  Notice that it has two parameters, the Coord the fact relates to, and the MineProp
  that applies to that cell.
* MineFact has constructors for each known mine or non-mine - these are MineFacts
  you can treat as axiomatically true, and use to build other facts.
* Since KnownNotMine is just like IsNotMine, except it provides more information,
  you can convert a fact to the IsNotMine form using the KnownNotMineIsNotMine
  constructor.
* The AllMinesAccountedFor constructor lets you prove that a cell is not a mine
  because a known non-mine cell with a number of mines is already touching enough
  known mines. For example, if a known non-mine is known to be touching exactly one
  mine, and you can prove that a cell touching the '1' is a mine, you can prove that
  all other cells touching the '1' are not mines using this constructor. To use it,
  you must specify the known non-mine coordinate, the coordinate for the cell you
  want to prove isn't a mine, the proof that the known non-mine is actually a known
  non-mine (usually taken from the NoMineAt... axioms), a list of known mine
  coordinates touching the mine, a proof that this list has the right length,
  a proof that the known mines are mines, a proof that the known mines are
  neighbours of the known non-mine, a proof that the coordinate you are
  constructing the proof for is a neighbour of the known non-mine, and a proof
  that the coordinate is not one of the supplied known mines.
* There is a similar constructor, AllNonMinesAccountedFor, that lets you prove that
  a cell is a mine because it is a neighbour of known non-mine, and enough of its
  neighbours are provably not mines. For example, if there was a known non-mine
  touching '6' mines (with 8 neighbours), and you can prove two neighbours are not
  mines, you can prove anyo f the remaining 6 neighbours are mines with this
  constructor.

## Uniqueness of status

Four additional axioms are provided (currently constructed with believe_me), on the
premise that it should be impossible to improve that the same cell is both a mine
and a non-mine. These are useful for proofs by contradiction.

* notNonMineImpliesMine - if you can disprove that a cell is not a mine, you can prove it is a mine.
* notMineImpliesNonMine - if you can disprove that a cell is a mine, you can prove it is a non-mine.
* nonMineImpliesNotMine - if you can prove a cell is not a mine, you can disprove that it is a mine.
* mineImpliesNotNonMine - if you can prove a cell is a mine, you can disprove that it is a non-mine.

## Using ProofSweeperLemmas

The ProofSweeperLemmas file comes with some useful lemmas that can help you to prove
things.

eqTestIsEqCoord is a useful lemma that proves that if use compare two coordinates, and
the comparison comes out true, then the coordinates are the same. Since list
operations rely on comparison, and the axioms use lists, this is a very useful lemma.

ifWithProof lets you handle the true and false case, and passes in a proof that it
is in fact true or false to each branch.

trueForAllListElems1 through to trueForAllListElems8 let you deal with statements
of the form: for all v, given that v is in some list, then some proposition p holds for all v. You need to know the length of the list (1 through to 8), and supply the elements of the list, plus a proof for each element of the list. You also need to supply the proposition p, and the proof that an equality comparison returning true means the elements are equal. This is most usefully used with lists of Coords - in which case you can use eqTestIsEqCoord to provide the proof that true equality comparison means equality.

## A simple example proof

The top of ProofSweeperPlay-Example.idr provides a useful template for constructing
your own ProofSweeperPlay.idr to play the game. It also shows two common tactics
you can use to prove things in ProofSweeper.

In the first example, we want to prove that `(14,9)` is a mine. On a 15x15 board,
`(14,9)` is right up against the right wall, so it only has 5 neighbours. It was
known axiomatically that `(14,10)` is a non-mine with one neighbour. It was also
known axiomatically that the other 4 neighbours (apart from `(14,9)`), namely
`(13,9)`, `(13,10)`, `(13,11)`, and `(14,11)`, are not mines. Using the
AllMinesAccountedFor constructor, it was possible to construct
`MineFact (MkCoord 14 9) IsMine` and run `ProofSweeperEngine move mine 14 9` to
check the proof and mark `(14,9)` as a mine. Notice the naming of the
proof as `mineAt_14_9` - ProofSweeperEngine expects this naming scheme to be followed.

## A proof by contradiction

Further down in the example, a more complex proof is constructed. The layout of
mines means that x cannot possibly be a mine, because that would create a
contradiction. Refer to the comments in ProofSweeperPlay-Example.idr to see how
the player assumed that `XCoord` (`(12,13)`) was a mine, and proved that this led
to a contradiction (leading to the construction of Void). This contradiction
disproved that it was a mine, and notMineImpliesNonMine was used to construct a proof
that XCoord was not a mine.
