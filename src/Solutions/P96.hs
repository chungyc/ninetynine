{- |
Description: Syntax checking
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P96" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P96 (isIdentifier) where

import qualified Data.Set as Set

{- |
Identifiers in the [Ada programming language](https://www.adaic.org/resources/add_content/standards/05rm/html/RM-2-3.html)
have the syntax described by the diagram below.

![Ada identifier syntax as explained in https://perso.telecom-paristech.fr/pautet/Ada95/chap01.htm](images/Miscellaneous/Ada.svg)

Write a function which checks whether a given string is a legal identifier.
-}
isIdentifier :: String -> Bool
isIdentifier = consumeFirstLetter

consumeFirstLetter :: String -> Bool
consumeFirstLetter [] = False
consumeFirstLetter [x]
  | isLetter x = True
  | otherwise  = False
consumeFirstLetter (x:'_':xs)
  | isLetter x = consumeLetterDigit xs
  | otherwise  = False
consumeFirstLetter (x:xs)
  | isLetter x = consumeLetterDigit xs
  | otherwise  = False

consumeLetterDigit :: String -> Bool
consumeLetterDigit [] = False
consumeLetterDigit [x]
  | isLetter x = True
  | isDigit x  = True
  | otherwise  = False
consumeLetterDigit (x:'_':xs)
  | isLetter x = consumeLetterDigit xs
  | isDigit x  = consumeLetterDigit xs
  | otherwise  = False
consumeLetterDigit (x:xs)
  | isLetter x = consumeLetterDigit xs
  | isDigit x  = consumeLetterDigit xs
  | otherwise  = False

isLetter :: Char -> Bool
isLetter x = Set.member x letters
  where letters = Set.fromList $ ['A'..'Z'] ++ ['a'..'z']

isDigit :: Char -> Bool
isDigit x = Set.member x digits
  where digits = Set.fromList ['0'..'9']
