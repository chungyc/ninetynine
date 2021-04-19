{- |
Description: Universal logic gates
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P47" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P47 (evaluateCircuit, buildCircuit) where

import           Data.List     (nub)
import           Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as Map
import           Data.Set      (Set)
import qualified Data.Set      as Set

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
-}
evaluateCircuit :: [(Int,Int)] -> Bool -> Bool -> Bool
evaluateCircuit circuit x y = eval 1 circuit [(-2,y), (-1,x)]

eval :: Int -> [(Int,Int)] -> [(Int,Bool)] -> Bool
eval _ [] []                   = undefined
eval _ [] (r:_)                = snd r
eval i ((k,l):circuit) outputs = eval (i+1) circuit $ (i, nand x y) : outputs
  where (Just x) = lookup k outputs
        (Just y) = lookup l outputs

nand :: Bool -> Bool -> Bool
nand x y = not $ x && y

{- |
Any boolean function can be computed with logic circuits composed solely of NAND gates.
I.e., NAND is a universal logic gate.

Write a function to return a logic circuit composed of NAND gates
which computes a given a binary boolean function.
The logic circuit should be in a form which 'evaluateCircuit' can evaluate.
-}
buildCircuit :: (Bool -> Bool -> Bool) -> [(Int,Int)]
buildCircuit f = indexCircuit c
  where c = circuits ! (f False False, f False True, f True False, f True True)

-- | Turns gates identified by the truth tables they produce into numerical indexes in the list.
indexCircuit :: [(Table,Table)] -> [(Int,Int)]
indexCircuit c = reverse $ indexCircuit' c rawinputs 1 []
  where rawinputs = Map.fromList [(inputLeft, -1), (inputRight, -2)]

indexCircuit' :: [(Table,Table)] -> Map Table Int -> Int -> [(Int,Int)] -> [(Int,Int)]
indexCircuit' [] _ _ c         = c
indexCircuit' ((l,r):gs) m i c = indexCircuit' gs m' i' c'
  where m' = Map.insert (nandTable l r) i m
        i' = i + 1
        c' = (m ! l, m ! r) : c

-- | Representation of a binary boolean function with a truth table.
--
-- The values are indexed by the input values in
-- the order of  @[(False, False), (False, True), (True, False), (True, True)]@.
type Table = (Bool, Bool, Bool, Bool)

-- | All possible truth tables.
tables :: [Table]
tables = [(x,y,z,w) | x <- bools, y <- bools, z <- bools, w <- bools]
  where bools = [False, True]

-- | The truth table for the output of a NAND gate,
-- given that the outputs of the NAND gates serving as inputs
-- have the given truth tables.
--
-- The truth tables are indexed by the inputs to the entire logic circuit,
-- not the direct inputs into a gate.
-- Truth tables based on the latter would all be identical.
nandTable :: Table -> Table -> Table
nandTable (x,y,z,w) (x',y',z',w') = (x `nand` x', y `nand` y', z `nand` z', w `nand` w')

-- | Truth table which behaves identically to the first input boolean variable.
-- I.e., the truth table for @\x -> \_ -> x@.
inputLeft :: Table
inputLeft = (False, False, True, True)

-- | Truth table which behaves identically to the second input boolean variable.
-- I.e., the truth table for @\_ -> \x -> x@.
inputRight :: Table
inputRight = (False, True, False, True)

-- | Maps a truth table to a circuit producing the truth table.
--
-- Due to the way they are constructed, while the circuits may not have the minimum
-- number of gates possible, by induction they will be as shallow as possible.
circuits :: Map Table [(Table,Table)]
circuits = Map.map nub $ build directCircuits remaining
  where gates = Set.fromList [(x,y) | x <- tables, y <- tables]
        directGates = [(x,y) | x <- [inputLeft, inputRight], y <- [inputLeft, inputRight]]
        directCircuits = Map.fromList $ map (\g@(l,r) -> (l `nandTable` r, [g])) directGates
        remaining = Set.difference gates $ Set.fromList directGates

-- | Build logic circuits for all possible binary boolean functions.
--
-- From a minimal set of gates directly connected to the input boolean variables,
-- it will compute what truth tables can be produced by feeding their outputs
-- to a NAND gate, which also provides the circuits which can compute the new truth tables.
-- This will be repeated until all possible outputs have been combined with a NAND gate,
-- during which logic circuits for all truth tables would be derived.
--
-- This takes adavantage of the fact that NAND is a universal logic gate.
-- For a logic gate that is not universal, it would reach a point where
-- it cannot produce a circuit for a set of remaining truth tables.
--
-- The same gate may appear multiple times in a circuit;
-- keeping the first one ensures that the circuit is still valid.
build :: Map Table [(Table,Table)]  -- ^ Circuits built so far.
      -> Set (Table,Table)          -- ^ Pair of inputs that have not been tried yet.
      -> Map Table [(Table,Table)]  -- ^ Circuits for all truth tables.
build built remaining
  | Set.null remaining = built
  | otherwise = build built' remaining'
  where (inputs, remaining') = Set.partition ready remaining
        ready (l,r) = Map.member l built && Map.member r built
        built' = Map.union built $ Map.fromList $ map extend $ Set.toList inputs
        extend g@(l,r) = (l `nandTable` r, (built ! l) ++ (built ! r) ++ [g])
