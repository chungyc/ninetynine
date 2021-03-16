{-|
Description: Ninety-Nine Haskell Problems
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

This is a list of Ninety-Nine Haskell Problems.
Some solutions to these problems are in "Solutions".

The number of shruggies indicate difficulty level, and are based on the original list.
These difficulty levels were based on the original list of Prolog problems,
so they may not be appropriate in the context of Haskell.

These are based on the Ninety-Nine Haskell Problems on the [HaskellWiki](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems).
The source for this module is available at https://github.com/chungyc/ninetynine.
-}
module Problems (
  -- * List problems
  -- ** Problem 1
  -- | &#129335;
  myLast,
  -- ** Problem 2
  -- | &#129335;
  myButLast,
  -- ** Problem 3
  -- | &#129335;
  elementAt,
  -- ** Problem 4
  -- | &#129335;
  myLength,
  -- ** Problem 5
  -- | &#129335;
  myReverse,
  -- ** Problem 6
  -- | &#129335;
  isPalindrome,
  -- ** Problem 7
  -- | &#129335; &#129335;
  flatten,
  NestedList (Elem, List),
  -- ** Problem 8
  -- | &#129335; &#129335;
  compress,
  -- ** Problem 9
  -- | &#129335; &#129335;
  pack,
  -- ** Problem 10
  -- | &#129335;
  encode,
  -- ** Problem 11
  -- | &#129335;
  encodeModified,
  Encoding (Single, Multiple),
  -- ** Problem 12
  -- | &#129335; &#129335;
  decodeModified,
  -- ** Problem 13
  -- | &#129335; &#129335;
  encodeDirect,
  -- ** Problem 14
  -- | &#129335;
  dupli,
  -- ** Problem 15
  -- | &#129335; &#129335;
  repli,
  -- ** Problem 16
  -- | &#129335; &#129335;
  dropEvery,
  -- ** Problem 17
  -- | &#129335;
  split,
  -- ** Problem 18
  -- | &#129335; &#129335;
  slice,
  -- ** Problem 19
  -- | &#129335; &#129335;
  rotate,
  -- ** Problem 20
  -- | &#129335;
  removeAt,
  -- ** Problem 21
  insertAt,
  -- ** Problem 22
  range,
  -- ** Problem 23
  randomSelect,
  -- ** Problem 24
  randomDraw,
  -- ** Problem 25
  randomPermute,
  -- * Arithmetic problems
  -- ** Problem 31
  -- | &#129335; &#129335;
  isPrime,
  -- ** Problem 32
  -- | &#129335; &#129335;
  myGCD,
  -- ** Problem 33
  -- | &#129335;
  coprime,
  -- ** Problem 34
  -- | &#129335; &#129335;
  totient,
  -- ** Problem 35
  -- | &#129335; &#129335;
  primeFactors,
  -- ** Problem 36
  -- | &#129335; &#129335;
  primeFactorsMultiplicity,
  -- ** Problem 37
  -- | &#129335; &#129335;
  totient',
  -- ** Problem 39
  -- | &#129335;
  primesR,
  primes,
  -- ** Problem 40
  -- | &#129335; &#129335;
  goldbach,
  -- * Logic and code problems
  -- ** Problem 46
  -- | &#129335; &#129335;
  BoolFunc,
  table,
  -- ** Problem 48
  -- | &#129335; &#129335;
  tablen,
  -- ** Problem 49
  -- | &#129335; &#129335;
  gray,
  -- * Binary tree problems
  -- ** Problem 54
  -- | &#129335;
  Tree (Empty, Branch),
  leaf,
  tree1,
  tree2,
  tree3,
  tree4,
  -- ** Problem 55
  -- | &#129335; &#129335;
  completelyBalancedTrees,
  -- ** Problem 56
  -- | &#129335; &#129335;
  symmetric,
  -- ** Problem 57
  -- | &#129335; &#129335;
  construct,
  addedTo,
  -- ** Problem 58
  -- | &#129335; &#129335;
  symmetricBalancedTrees,
  -- ** Problem 59
  -- | &#129335; &#129335;
  heightBalancedTrees,
  -- ** Problem 60
  -- | &#129335; &#129335;
  heightBalancedTreesWithNodes,
  -- ** Problem 61
  leaves,
  internals,
  -- ** Problem 62
  atLevel,
  -- ** Problem 63
  completeBinaryTree,
  isCompleteBinaryTree,
  -- ** Problem 64
  layoutInorder,
  -- * Multiway tree problems
  MultiwayTree (MultiwayTree),
  multitree1,
  multitree2,
  multitree3,
  multitree4,
  multitree5,
  -- ** Problem 70
  -- | &#129335; &#129335;
  stringToTree,
  treeToString,
  -- ** Problem 71
  -- | &#129335;
  internalPathLength,
  -- ** Problem 72
  -- | &#129335;
  postOrderSequence,
  -- ** Problem 73
  -- | &#129335; &#129335;
  treeToSexp,
  sexpToTree,
  -- * Graph problems
  Graph (vertexes, edges),
  Vertex,
  Edge (Edge),
  Var,
  Lists (Lists),
  Adjacency (Adjacency),
  Paths (Paths),
  G (G),
  -- ** Problem 80
  -- | &#129335; &#129335; &#129335;
  ConvertibleGraph,
  toLists,
  toAdjacency,
  toPaths,
  toG,
  -- ** Problem 81
  -- | &#129335; &#129335;
  paths,
  -- ** Problem 82
  -- | &#129335; &#129335;
  cycles,
  -- ** Problem 83
  -- | &#129335; &#129335;
  spanningTrees,
  isTree,
  isConnected,
  -- ** Problem 84
  -- | &#129335; &#129335;
  minimumSpanningTree,
  -- ** Problem 85
  -- | &#129335; &#129335;
  isomorphic,
  -- ** Problem 86
  -- | &#129335; &#129335;
  colorGraph,
  -- * Miscellaenous problems
  -- ** Problem 90
  -- | &#129335; &#129335;
  queens,
  -- ** Problem 91
  -- | &#129335; &#129335;
  knightsTour,
  closedKnightsTour,
  -- ** Problem 95
  -- |  &#129335; &#129335;
  fullWords,
  -- ** Problem 97
  -- |  &#129335; &#129335;
  sudoku,
  ) where

import           Problems.BinaryTrees
import           Problems.Graphs
import           Problems.Lists
import           Problems.MultiwayTrees
import           Problems.P01
import           Problems.P02
import           Problems.P03
import           Problems.P04
import           Problems.P05
import           Problems.P06
import           Problems.P07
import           Problems.P08
import           Problems.P09
import           Problems.P10
import           Problems.P11
import           Problems.P12
import           Problems.P13
import           Problems.P14
import           Problems.P15
import           Problems.P16
import           Problems.P17
import           Problems.P18
import           Problems.P19
import           Problems.P20
import           Problems.P21
import           Problems.P22
import           Problems.P23
import           Problems.P24
import           Problems.P25
import           Problems.P31
import           Problems.P32
import           Problems.P33
import           Problems.P34
import           Problems.P35
import           Problems.P36
import           Problems.P37
import           Problems.P39
import           Problems.P40
import           Problems.P46
import           Problems.P48
import           Problems.P49
import           Problems.P54
import           Problems.P55
import           Problems.P56
import           Problems.P57
import           Problems.P58
import           Problems.P59
import           Problems.P60
import           Problems.P61
import           Problems.P62
import           Problems.P63
import           Problems.P64
import           Problems.P70
import           Problems.P71
import           Problems.P72
import           Problems.P73
import           Problems.P80
import           Problems.P81
import           Problems.P82
import           Problems.P83
import           Problems.P84
import           Problems.P85
import           Problems.P86
import           Problems.P90
import           Problems.P91
import           Problems.P95
import           Problems.P97
