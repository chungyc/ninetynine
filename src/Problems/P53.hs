{- |
Description: Resolution rule
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P53".
-}
module Problems.P53 where

import           Problems.Logic (Formula (..))
import qualified Solutions.P53  as Solution

{- |

=== Examples

>>> :{
isTheorem
  [ Variable "X", Variable "Y", Variable "Z" ] $
  Disjoin [ Variable "X", Variable "Y" ]
:}
True

>>> :{
    isTheorem
      [ Disjoin [ Complement $ Variable "X", Variable "Y" ]
      , Disjoin [ Complement $ Variable "Y", Variable "Z" ]
      ]
      (Disjoin [ Complement $ Variable "X", Variable "Z" ])
:}
True

>>> isTheorem [ Variable "X" ] $ Variable "Y"
False
-}
isTheorem :: [Formula] -> Formula -> Bool
isTheorem = Solution.isTheorem
