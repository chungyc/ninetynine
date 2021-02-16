{-|
Description: Solutions to Ninety-Nine Haskell Problems
Copyright: Copyright (C) 2021 Yoo Chung
License: BSD3
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
  repli
  ) where

import           Problems.P07.Definitions
import           Problems.P11.Definitions
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