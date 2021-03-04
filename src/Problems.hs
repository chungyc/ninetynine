{-|
Description: Ninety-Nine Haskell Problems
Copyright: Copyright (C) 2021 Yoo Chung
License: BSD3
Maintainer: dev@chungyc.org

This is a list of Ninety-Nine Haskell Problems.
The number of shruggies indicate difficulty level, and are based on the original list.
Some solutions to these problems are in "Solutions".

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
  cbalTree,
  -- ** Problem 56
  -- | &#129335; &#129335;
  symmetric,
  -- ** Problem 57
  -- | &#129335; &#129335;
  construct,
  addedTo,
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
  --
  -- Write functions to convert between the different graph representations
  -- 'Lists', 'Adjacency', 'Paths', and 'G'.
  --
  -- The types can already be easily converted between each other using
  -- the 'sets' and 'toGraph' functions available to the 'Graph' type class.
  -- Unlike other problems, this problem should be solved without using
  -- the functions available to the 'Graph' type class for it to not be trivial.
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
  -- * Miscellaenous problems
  -- ** Problem 90
  -- | &#129335; &#129335;
  queens,
  ) where

import           Problems.BinaryTrees
import           Problems.Graphs
import           Problems.Lists
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
import           Problems.P31
import           Problems.P32
import           Problems.P33
import           Problems.P34
import           Problems.P35
import           Problems.P36
import           Problems.P37
import           Problems.P39
import           Problems.P46
import           Problems.P48
import           Problems.P49
import           Problems.P54
import           Problems.P55
import           Problems.P56
import           Problems.P57
import           Problems.P80
import           Problems.P81
import           Problems.P82
import           Problems.P83
import           Problems.P90
