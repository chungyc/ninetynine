{-|
Description: Ninety-Nine Haskell Problems
Copyright: Copyright (C) 2021 Yoo Chung
License: BSD3
Maintainer: dev@chungyc.org

This is a list of [Ninety-Nine Haskell Problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems).
The number of shruggies indicate difficulty level, and are based on the original list.
Some solutions to these problems are in "Solutions".

The source for this module is available at https://github.com/chungyc/ninetynine.
-}
module Problems (
  -- * List problems
  -- ** Problem 1
  myLast,
  -- ** Problem 2
  myButLast,
  -- ** Problem 3
  elementAt,
  -- ** Problem 4
  myLength,
  -- ** Problem 5
  myReverse,
  -- ** Problem 6
  isPalindrome,
  -- ** Problem 7
  flatten,
  NestedList (Elem, List),
  -- ** Problem 8
  compress,
  -- ** Problem 9
  pack,
  -- ** Problem 10
  encode,
  -- ** Problem 11
  encodeModified,
  Encoding (Single, Multiple),
  -- ** Problem 12
  decodeModified,
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
  -- * Arithmetic problems
  -- ** Problem 31
  isPrime,
  -- ** Problem 32
  myGCD,
  -- ** Problem 33
  coprime,
  -- ** Problem 34
  totient,
  -- * Logic and code problems
  -- ** Problem 46
  BoolFunc,
  table,
  -- * Binary tree problems
  -- ** Problem 54
  Tree (Empty, Branch),
  leaf,
  tree1,
  tree2,
  tree3,
  tree4,
  -- * Miscellaenous problems
  -- ** Problem 90
  queens,
  ) where

import           Problems.P01
import           Problems.P02
import           Problems.P03
import           Problems.P04
import           Problems.P05
import           Problems.P06
import           Problems.P07
import           Problems.P07.Definitions
import           Problems.P08
import           Problems.P09
import           Problems.P10
import           Problems.P11
import           Problems.P11.Definitions
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
import           Problems.P46
import           Problems.P54
import           Problems.P54.Definitions
import           Problems.P90
