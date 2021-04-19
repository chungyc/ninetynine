{- |
Description: Universal logic gates
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P47".
-}
module Problems.P47 where

import qualified Solutions.P47 as Solution

{- |
Consider a logic circuit composed of NAND gates with binary input, specified as follows.

* A list of pairs of numbers for each NAND gate.

* A pair @(i,j)@ in the list denotes that the inputs for the NAND gate
  are the outputs for the @i@th and @j@th gates in the list, respectively,
  with the first element starting at index 1.

    * As a special case, -1 and -2 denote the first and second boolean variables
      fed into the circuit, respectively.

    * An input to a NAND gate can only use output from a NAND gate which appears earlier in the list.

* The output of the last gate in the list is the output of the logic circuit.

Write a function to evaluate a given logic circuit as specified above.

=== Examples

>>> evaluateCircuit [(-1,-2)] True True
False

>>> evaluateCircuit [(-1,-2)] True False
True

>>> evaluateCircuit [(-1,-2), (1,1)] True True
True

>>> evaluateCircuit [(-1,-2), (1,1)] True False
False

>>> evaluateCircuit [(-1,-1),(-2,-2),(1,2)] True False
True

>>> evaluateCircuit [(-1,-1),(-2,-2),(1,2)] False False
False
-}
evaluateCircuit :: [(Int,Int)] -> Bool -> Bool -> Bool
evaluateCircuit = Solution.evaluateCircuit

{- |
Any boolean function can be computed with logic circuits composed solely of NAND gates.
I.e., NAND is a universal logic gate.

Write a function to return a logic circuit composed of NAND gates
which computes a given a binary boolean function.
The logic circuit should be in a form which 'evaluateCircuit' can evaluate.

=== Examples

>>> evaluateCircuit (buildCircuit (&&)) False False
False

>>> evaluateCircuit (buildCircuit (&&)) False True
False

>>> evaluateCircuit (buildCircuit (&&)) True False
False

>>> evaluateCircuit (buildCircuit (&&)) True True
True

=== __Hint__

There are only 16 binary boolean functions, ignoring how they are actually computed,
so it is feasible to devise logic circuits for all of them manually.

To have a computer construct them automatically, which may be more difficult,
take advantage of the fact that a boolean function can be represented by its truth table.
Consider what the inputs must be like for a NAND gate to output a desired truth table.
-}
buildCircuit :: (Bool -> Bool -> Bool) -> [(Int,Int)]
buildCircuit = Solution.buildCircuit
