{-|
Description: Ninety-Nine Haskell Problems
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

This is a list of Ninety-Nine Haskell Problems.

Each problem is provided a skeleton where you can implement your own solutions.
For example, the "Problems.P11" module provides a skeleton in which you can
provide your own implementation.  Once implemented, you can run the existing tests
to confirm it is indeed a correct solution, or at least one without obvious bugs.
You can also run existing benchmarks against your solution, e.g.,
if you are curious how your amazingly clever but complicated solution performs
compared to a simpler one.

Some solutions to these problems are in the "Solutions" module.
The number of shruggies (&#129335;) indicate difficulty level.
These difficulty levels are based on the original list of Prolog problems,
so they may not be appropriate in the context of Haskell.

These are based on the Ninety-Nine Haskell Problems on
the [HaskellWiki](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems).
The source for this module is available at https://github.com/chungyc/ninetynine.
-}
module Problems (
  -- * List problems
  -- ** Problem 1
  -- | == &#129335; Last element of a list
  myLast,
  -- ** Problem 2
  -- | == &#129335; Penultimate element of a list
  myButLast,
  -- ** Problem 3
  -- | == &#129335; Find element of a list at a given position
  elementAt,
  -- ** Problem 4
  -- | == &#129335; Length of a list
  myLength,
  -- ** Problem 5
  -- | == &#129335; Reverse a list
  myReverse,
  -- ** Problem 6
  -- | == &#129335; Palindromes
  isPalindrome,
  -- ** Problem 7
  -- | == &#129335; &#129335; Flatten a nested list structure
  flatten,
  NestedList (Elem, List),
  -- ** Problem 8
  -- | == &#129335; &#129335; Eliminate duplicate elements in a list
  compress,
  -- ** Problem 9
  -- | == &#129335; &#129335; Pack duplicates in a list
  pack,
  -- ** Problem 10
  -- | == &#129335; Run-length encoding of a list
  encode,
  -- ** Problem 11
  -- | == &#129335; Modified run-length encoding
  encodeModified,
  Encoding (Single, Multiple),
  -- ** Problem 12
  -- | == &#129335; &#129335; Decode a run-length encoded list
  decodeModified,
  -- ** Problem 13
  -- | == &#129335; &#129335; Run-length encoding of a list; direct solution
  encodeDirect,
  -- ** Problem 14
  -- | == &#129335; Duplicate elements in a list
  dupli,
  -- ** Problem 15
  -- | == &#129335; &#129335; Replicate elements of a list
  repli,
  -- ** Problem 16
  -- | == &#129335; &#129335; Drop elements in a list
  dropEvery,
  -- ** Problem 17
  -- | == &#129335; Split a list
  split,
  -- ** Problem 18
  -- | == &#129335; &#129335; Extract a slice from a list
  slice,
  -- ** Problem 19
  -- | == &#129335; &#129335; Rotate a list
  rotate,
  -- ** Problem 20
  -- | == &#129335; Remove element from a list
  removeAt,
  -- ** Problem 21
  -- | == &#129335; Insert element into a list
  insertAt,
  -- ** Problem 22
  -- | == &#129335; Range of integers
  range,
  -- ** Problem 23
  -- | == &#129335; &#129335; Select random elements from a list
  randomSelect,
  -- ** Problem 24
  -- | == &#129335; Lotto
  randomDraw,
  -- ** Problem 25
  -- | == &#129335; Random permutation of a list
  randomPermute,
  -- ** Problem 26
  -- | == &#129335; &#129335; Combinations
  combinations,
  -- ** Problem 27
  -- | == &#129335; &#129335; Group the elements of a set into disjoint subsets
  disjointGroups,
  -- ** Problem 28
  -- | == &#129335; &#129335; Sorting a list of lists according to length of sublists
  lsort,
  lfsort,
  -- * Arithmetic problems
  -- ** Problem 31
  -- | == &#129335; &#129335; Primality checking
  isPrime,
  -- ** Problem 32
  -- | == &#129335; &#129335; Greatest common divisor
  myGCD,
  -- ** Problem 33
  -- | == &#129335; Coprimality
  coprime,
  -- ** Problem 34
  -- | == &#129335; &#129335; Euler's totient function
  totient,
  -- ** Problem 35
  -- | == &#129335; &#129335; List of prime factors
  primeFactors,
  -- ** Problem 36
  -- | == &#129335; &#129335; List of prime factors and their multiplicities
  primeFactorsMultiplicity,
  -- ** Problem 37
  -- | == &#129335; &#129335; Euler's totient function with Euler's product formula
  totient',
  -- ** Problem 39
  -- | == &#129335; List of prime numbers
  primesR,
  primes,
  -- ** Problem 40
  -- | == &#129335; &#129335; Goldbach's conjecture
  goldbach,
  -- ** Problem 41
  -- | == &#129335; &#129335; List of Goldbach pairs
  goldbachList,
  -- * Logic and code problems
  -- ** Problem 46
  -- | == &#129335; &#129335; Truth tables for logical expressions
  BoolFunc,
  table,
  -- ** Problem 48
  -- | == &#129335; &#129335; Truth tables for \(n\)-ary boolean functions
  tablen,
  -- ** Problem 49
  -- | == &#129335; &#129335; Gray codes
  gray,
  -- ** Problem 50
  -- | == &#129335; &#129335; &#129335; Huffman codes
  huffman,
  -- * Binary tree problems
  -- ** Problem 54
  -- | == &#129335; Binary trees
  Tree (Empty, Branch),
  leaf,
  tree1,
  tree2,
  tree3,
  tree4,
  -- ** Problem 55
  -- | == &#129335; &#129335; Construct completely balanced binary trees
  completelyBalancedTrees,
  -- ** Problem 56
  -- | == &#129335; &#129335; Symmetric binary trees
  symmetric,
  -- ** Problem 57
  -- | == &#129335; &#129335; Binary search trees
  construct,
  addedTo,
  -- ** Problem 58
  -- | == &#129335; &#129335; Symmetric and completely balanced binary trees
  symmetricBalancedTrees,
  -- ** Problem 59
  -- | == &#129335; &#129335; Construct height-balanced binary trees
  heightBalancedTrees,
  -- ** Problem 60
  -- | == &#129335; &#129335; Height-balanced binary trees with given number of nodes
  heightBalancedTreesWithNodes,
  -- ** Problem 61
  -- | == &#129335; Collect nodes of a binary tree
  leaves,
  internals,
  -- ** Problem 62
  -- | == &#129335; Collect nodes at a given level
  atLevel,
  -- ** Problem 63
  -- | == &#129335; &#129335; Construct a complete binary tree
  completeBinaryTree,
  isCompleteBinaryTree,
  -- ** Problem 64
  -- | == &#129335; &#129335; Binary tree layout; in-order
  layoutInorder,
  -- ** Problem 65
  -- | == &#129335; &#129335; Binary tree layout; constant distance at each level
  layoutLevelConstant,
  -- ** Problem 66
  -- | ==  &#129335; &#129335; &#129335; Binary tree layout; compact
  layoutCompact,
  -- ** Problem 67
  -- | == &#129335; &#129335; A string representation of binary trees
  treeToString,
  stringToTree,
  -- ** Problem 68
  -- | == &#129335; &#129335; In-order and pre-order sequences of binary trees
  inorder,
  preorder,
  ordersToTree,
  -- ** Problem 69
  -- | == &#129335; &#129335; Dotstring representation of binary trees
  dotstringToTree,
  treeToDotstring,
  -- * Multiway tree problems
  MultiwayTree (MultiwayTree),
  multitree1,
  multitree2,
  multitree3,
  multitree4,
  multitree5,
  -- ** Problem 70
  -- | == &#129335; &#129335; Tree construction from a node string
  stringToMultitree,
  multitreeToString,
  -- ** Problem 71
  -- | == &#129335; Internal path length of a tree
  internalPathLength,
  -- ** Problem 72
  -- | == &#129335; Post-order sequence of a tree
  postOrderSequence,
  -- ** Problem 73
  -- | == &#129335; &#129335; Tree representation with s-expressions
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
  -- | == &#129335; &#129335; &#129335; Converting between graph representations
  ConvertibleGraph,
  toLists,
  toAdjacency,
  toPaths,
  toG,
  -- ** Problem 81
  -- | == &#129335; &#129335; Paths between vertexes
  paths,
  -- ** Problem 82
  -- | == &#129335; &#129335; Cycles with a given vertex
  cycles,
  -- ** Problem 83
  -- | == &#129335; &#129335; Construct spanning trees
  spanningTrees,
  isTree,
  isConnected,
  -- ** Problem 84
  -- | == &#129335; &#129335; Construct minimum spanning tree
  minimumSpanningTree,
  -- ** Problem 85
  -- | == &#129335; &#129335; Graph isomorphism
  isomorphic,
  -- ** Problem 86
  -- | == &#129335; &#129335; Graph coloring
  colorGraph,
  -- ** Problem 87
  -- | == &#129335; &#129335; Depth-first graph traversal
  depthFirst,
  -- ** Problem 88
  -- | == &#129335; &#129335; Connected components
  connectedComponents,
  -- ** Problem 89
  -- | == &#129335; &#129335; Bipartite graphs
  bipartite,
  -- * Miscellaenous problems
  -- ** Problem 90
  -- | == &#129335; &#129335; Find all solutions to the \(n\) queens problem
  queens,
  -- ** Problem 91
  -- | == &#129335; &#129335; Knight's tour
  knightsTour,
  closedKnightsTour,
  -- ** Problem 92
  -- | == &#129335; &#129335; &#129335; Graceful tree labeling
  gracefulTree,
  -- ** Problem 93
  -- | == &#129335; &#129335; &#129335; An arithmetic puzzle
  arithmeticPuzzle,
  -- ** Problem 94
  -- | == &#129335; &#129335; &#129335; Regular graphs
  regularGraphs,
  -- ** Problem 95
  -- | == &#129335; &#129335; English number words
  fullWords,
  -- ** Problem 97
  -- | == &#129335; &#129335; Sudoku
  sudoku,
  -- ** Problem 98
  -- | == &#129335; &#129335; &#129335; Nonograms
  nonogram,
  -- ** Problem 99
  -- | == &#129335; &#129335; &#129335; Crossword puzzles
  solveCrossword,
  Crossword (..),
  ) where

import           Problems.BinaryTrees
import           Problems.Crosswords
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
import           Problems.P26
import           Problems.P27
import           Problems.P28
import           Problems.P31
import           Problems.P32
import           Problems.P33
import           Problems.P34
import           Problems.P35
import           Problems.P36
import           Problems.P37
import           Problems.P39
import           Problems.P40
import           Problems.P41
import           Problems.P46
import           Problems.P48
import           Problems.P49
import           Problems.P50
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
import           Problems.P65
import           Problems.P66
import           Problems.P67
import           Problems.P68
import           Problems.P69
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
import           Problems.P87
import           Problems.P88
import           Problems.P89
import           Problems.P90
import           Problems.P91
import           Problems.P92
import           Problems.P93
import           Problems.P94
import           Problems.P95
import           Problems.P97
import           Problems.P98
import           Problems.P99
