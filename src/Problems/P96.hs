{- |
Description: Syntax checking
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P96".
-}
module Problems.P96 (isIdentifier) where

import qualified Solutions.P96 as Solution

{- |
Identifiers in the [Ada programming language](https://www.adaic.org/resources/add_content/standards/05rm/html/RM-2-3.html)
have the syntax described by the diagram below.

![Ada identifier syntax as explained in https://perso.telecom-paristech.fr/pautet/Ada95/chap01.htm](images/Miscellaneous/Ada.svg)

Write a function which checks whether a given string is a legal identifier.

=== Examples

>>> isIdentifier "this_is_a_long_identifier"
True
>>> isIdentifier "This_ends_in_an_underscore_"
False
>>> isIdentifier "This__has__two__consecutive__underscores"
False
>>> isIdentifier "1234"
False
>>> isIdentifier "_legal_in_many_other_languages"
False
>>> isIdentifier "Fibonacci_sequence_is_1_1_2_3_5_8_13_21_ad_infinitum"
True

=== __Hint__

Translate the syntax diagram into a recursive grammar.
-}
isIdentifier :: String -> Bool
isIdentifier = Solution.isIdentifier
