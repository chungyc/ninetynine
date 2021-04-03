{- |
Description: Binary tree layout; constant distance at each level
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P65".
-}
module Problems.P65 (layoutLevelConstant, tree65) where

import           Problems.BinaryTrees
import qualified Solutions.P65        as Solution

{- |
An alternative layout method is depicted in the illustration below:

![Layout example](images/BinaryTrees/Layout-P65.svg)

Find out the rules and write the corresponding function.
Use the same conventions as in "Problems.P64".

=== Examples

>>> layoutLevelConstant tree65
Branch ('n',(15,1)) (Branch ('k',(7,2)) (Branch ('c',(3,3)) ...

===  __Hint__

On a given level, the horizontal distance between neighboring nodes is constant.
-}
layoutLevelConstant :: Tree a -> Tree (a, (Int,Int))
layoutLevelConstant = Solution.layoutLevelConstant

-- | Tree used as the example for "Problems.P65".
tree65 :: Tree Char
tree65 = Branch 'n'
         (Branch 'k'
           (Branch 'c'
             (Branch 'a' Empty Empty)
             (Branch 'e'
               (Branch 'd' Empty Empty)
               (Branch 'g' Empty Empty)))
           (Branch 'm' Empty Empty))
         (Branch 'u'
           (Branch 'p'
             Empty
             (Branch 'q' Empty Empty))
           Empty)
