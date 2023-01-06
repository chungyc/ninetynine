{- |
Description: Highly totient numbers
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P38" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P38 (highlyTotientNumbers) where

import           Data.List    (genericLength, genericTake, unfoldr)
import           Problems.P35
import           Problems.P37

{- |
Construct the list of highly totient numbers.
-}
highlyTotientNumbers :: Integral a => [a]
highlyTotientNumbers = unfoldr find (0,0)

-- | Find the next highly totient number starting from the given totient number.
find :: Integral a => (a,a) -> Maybe (a,(a,a))
find (n, count) = Just (n', (n'+1, count'))
  where (n', count') = next (n, count)

next :: Integral a => (a,a) -> (a,a)
next (n, count) | count' > count = (n, count')
                | otherwise      = next (n+1, count)
  where count' = tally n

{- |
Count the number of solutions for a given totient number.

For \( x = \prod_{i=1}^r {p_i}^{k_i} = \prod_{i=1}^r {p_i}^{k_i - 1} p_i \),
the totient number is \( \phi(x) = \prod_{i=1}^r {p_i}^{k_i - 1} (p_i - 1).
The prime factorization of \(\phi(x)\) must be of the
form \( \left( \prod_{i=1}^r {p_i}^{k_i - 1} \right) \left( \prod_{i=1}^r \prod_{j=1}^{l_i} q_{ij} \right) \),
where \( p_i - 1 = \prod_{j=1}^{l_i} q_{ij} \) and \( q_{ij} \) is prime..

Consider the product of one added to 1 and all of the prime factors of \( \phi(x) \).
If there is a \( p_i = 2 \), then \( p_i - 1 < 1 \times 2 \).
It is also the case that for \( p_i > 2 \), \( p_i = 1 + \prod_{j=1}^{l_i} q_{ij} < \prod_{j=1}^{l_i} (q_ij + 1) \).
Therefore this product must be larger than \(x\).  I.e., it is an upper bound for \(x\) for a given \(n=\phi(x)\).

This means that if \(n\) has the prime factorization \( \prod_i q_i \),
then a solution \(x\) to \(\phi(x) = n\) must satisfy \( 1 \leq x \leq \prod_i (q_i+1) \).
There may well be a tighter bound we can compute from only \(n\),
but this is relatively simple without having to worry whether a prime factor \(q_i\) of \(n\)
is equal to a prime factor \(p_i\) of \(x\), or whether it is a prime factor of a \(p_i - 1\).
-}
tally :: Integral a => a -> a
tally n = genericLength $ filter ((==) n) $ genericTake bound totients
  where bound = product $ map (1+) $ 1 : primeFactors n

totients :: Integral a => [a]
totients = map totient' [1..]
