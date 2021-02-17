{-|
Description: Ninety-Nine Haskell Problems
Copyright: Copyright (C) 2021 Yoo Chung
License: BSD3
Maintainer: dev@chungyc.org

This is a list of [Ninety-Nine Haskell Problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems).
Some solutions to these problems are in "Solutions".
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
  NestedList (Elem, List),
  flatten,
  -- ** Problem 8
  compress,
  -- ** Problem 9
  pack,
  -- ** Problem 10
  encode,
  -- ** Problem 11
  Encoding (Single, Multiple),
  encodeModified,
  -- ** Problem 12
  decodeModified,
  -- ** Problem 13
  encodeDirect,
  -- ** Problem 14
  dupli,
  -- ** Problem 15
  repli,
  -- * Arithmetic problems
  -- ** Problem 31
  isPrime,
  -- ** Problem 32
  myGCD
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
import           Problems.P31
import           Problems.P32
