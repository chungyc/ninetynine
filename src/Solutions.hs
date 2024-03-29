{-|
Description: Solutions to Ninety-Nine Haskell Problems
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

These are some solutions for Ninety-Nine Haskell "Problems".
A problem may have more than one solution included.
-}
module Solutions (
  -- * List problems
  -- ** Problem 1
  myLast,
  -- ** Problem 2
  myButLast,
  -- ** Problem 3
  elementAt,
  -- ** Problem 4
  myLength,
  myLength',
  myLength'',
  myLength''',
  -- ** Problem 5
  myReverse,
  -- ** Problem 6
  isPalindrome,
  -- ** Problem 7
  flatten,
  flatten',
  -- ** Problem 8
  compress,
  -- ** Problem 9
  pack,
  pack',
  -- ** Problem 10
  encode,
  -- ** Problem 11
  encodeModified,
  -- ** Problem 12
  decodeModified,
  decodeModified',
  -- ** Problem 13
  encodeDirect,
  -- ** Problem 14
  dupli,
  -- ** Problem 15
  repli,
  -- ** Problem 16
  dropEvery,
  -- ** Problem 17
  split,
  -- ** Problem 18
  slice,
  slice',
  -- ** Problem 19
  rotate,
  -- ** Problem 20
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
  -- ** Problem 26
  combinations,
  -- ** Problem 27
  disjointGroups,
  -- ** Problem 28
  lsort,
  lfsort,
  -- * Arithmetic problems
  -- ** Problem 29
  fibonacci,
  -- ** Problem 30
  fibonacci',
  -- ** Problem 31
  isPrime,
  isPrime',
  isPrime'',
  -- ** Problem 32
  myGCD,
  -- ** Problem 33
  coprime,
  -- ** Problem 34
  totient,
  totientFiltered,
  -- ** Problem 35
  primeFactors,
  -- ** Problem 36
  primeFactorsMultiplicity,
  -- ** Problem 37
  totient',
  -- ** Problem 38
  highlyTotientNumbers,
  -- ** Problem 39
  primesR,
  primes,
  -- ** Problem 40
  goldbach,
  -- ** Problem 41
  goldbachList,
  -- ** Problem 42
  multiplicativeInverse,
  -- ** Problem 43
  gaussianDividesBy,
  -- ** Problem 44
  isGaussianPrime,
  -- ** Problem 45
  isGaussianPrime',
  -- * Logic and code problems
  -- ** Problem 46
  table,
  -- ** Problem 47
  evaluateCircuit,
  buildCircuit,
  -- ** Problem 48
  tablen,
  -- ** Problem 49
  gray,
  -- ** Problem 50
  huffman,
  -- ** Problem 51
  corrupt,
  errorCorrectingEncode,
  errorCorrectingDecode,
  -- ** Problem 52
  toConjunctiveNormalForm,
  -- ** Problem 53
  isTheorem,
  -- * Binary tree problems
  -- ** Problem 54
  Tree (Empty, Branch),
  leaf,
  tree1,
  tree2,
  tree3,
  tree4,
  -- ** Problem 55
  completelyBalancedTrees,
  -- ** Problem 56
  symmetric,
  -- ** Problem 57
  construct,
  addedTo,
  -- ** Problem 58
  symmetricBalancedTrees,
  -- ** Problem 59
  heightBalancedTrees,
  -- ** Problem 60
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
  -- ** Problem 65
  layoutLevelConstant,
  -- ** Problem 66
  layoutCompact,
  -- ** Problem 67
  treeToString,
  stringToTree,
  -- ** Problem 68
  inorder,
  preorder,
  ordersToTree,
  -- ** Problem 69
  dotstringToTree,
  treeToDotstring,
  -- * Multiway tree problems
  -- ** Problem 70
  stringToMultitree,
  multitreeToString,
  -- ** Problem 71
  internalPathLength,
  -- ** Problem 72
  postOrderSequence,
  -- ** Problem 73
  treeToSexp,
  sexpToTree,
  -- * Monad problems
  -- ** Problem 74
  askGoldbach,
  -- ** Problem 75
  maybeGoldbach,
  -- ** Problem 76
  eitherGoldbach,
  -- ** Problem 77
  randomWalkPaths,
  -- ** Problem 78
  collatz,
  -- ** Problem 79
  calculatePostfix,
  -- * Graph problems
  -- ** Problem 80
  ConvertibleGraph,
  toLists,
  toAdjacency,
  toPaths,
  toG,
  -- ** Problem 81
  paths,
  -- ** Problem 82
  cycles,
  -- ** Problem 83
  spanningTrees,
  isTree,
  isConnected,
  -- ** Problem 84
  minimumSpanningTree,
  -- ** Problem 85
  isomorphic,
  isomorphic',
  isomorphic'',
  -- ** Problem 86
  colorGraph,
  -- ** Problem 87
  depthFirst,
  -- ** Problem 88
  connectedComponents,
  -- ** Problem 89
  bipartite,
  -- * Miscellaenous problems
  -- ** Problem 90
  queens,
  -- ** Problem 91
  knightsTour,
  closedKnightsTour,
  -- ** Problem 92
  gracefulTree,
  gracefulTree',
  -- ** Problem 93
  arithmeticPuzzle,
  -- ** Problem 94
  regularGraphs,
  -- ** Problem 95
  fullWords,
  -- ** Problem 96
  isIdentifier,
  -- ** Problem 97
  sudoku,
  -- ** Problem 98
  nonogram,
  -- ** Problem 99
  solveCrossword,
  ) where

import           Problems.BinaryTrees
import           Solutions.P01
import           Solutions.P02
import           Solutions.P03
import           Solutions.P04
import           Solutions.P05
import           Solutions.P06
import           Solutions.P07
import           Solutions.P08
import           Solutions.P09
import           Solutions.P10
import           Solutions.P11
import           Solutions.P12
import           Solutions.P13
import           Solutions.P14
import           Solutions.P15
import           Solutions.P16
import           Solutions.P17
import           Solutions.P18
import           Solutions.P19
import           Solutions.P20
import           Solutions.P21
import           Solutions.P22
import           Solutions.P23
import           Solutions.P24
import           Solutions.P25
import           Solutions.P26
import           Solutions.P27
import           Solutions.P28
import           Solutions.P29
import           Solutions.P30
import           Solutions.P31
import           Solutions.P32
import           Solutions.P33
import           Solutions.P34
import           Solutions.P35
import           Solutions.P36
import           Solutions.P37
import           Solutions.P38
import           Solutions.P39
import           Solutions.P40
import           Solutions.P41
import           Solutions.P42
import           Solutions.P43
import           Solutions.P44
import           Solutions.P45
import           Solutions.P46
import           Solutions.P47
import           Solutions.P48
import           Solutions.P49
import           Solutions.P50
import           Solutions.P51
import           Solutions.P52
import           Solutions.P53
import           Solutions.P54
import           Solutions.P55
import           Solutions.P56
import           Solutions.P57
import           Solutions.P58
import           Solutions.P59
import           Solutions.P60
import           Solutions.P61
import           Solutions.P62
import           Solutions.P63
import           Solutions.P64
import           Solutions.P65
import           Solutions.P66
import           Solutions.P67
import           Solutions.P68
import           Solutions.P69
import           Solutions.P70
import           Solutions.P71
import           Solutions.P72
import           Solutions.P73
import           Solutions.P74
import           Solutions.P75
import           Solutions.P76
import           Solutions.P77
import           Solutions.P78
import           Solutions.P79
import           Solutions.P80
import           Solutions.P81
import           Solutions.P82
import           Solutions.P83
import           Solutions.P84
import           Solutions.P85
import           Solutions.P86
import           Solutions.P87
import           Solutions.P88
import           Solutions.P89
import           Solutions.P90
import           Solutions.P91
import           Solutions.P92
import           Solutions.P93
import           Solutions.P94
import           Solutions.P95
import           Solutions.P96
import           Solutions.P97
import           Solutions.P98
import           Solutions.P99
