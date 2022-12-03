{- |
Description: IO monad without do notation
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P74".
-}
module Problems.P74
  ( askGoldbach
  -- * Original function
  -- | The function below is the equivalent implementation of 'askGoldbach' using do notation.
  , askGoldbach'
  -- * Supporting function
  -- | The function below is not part of the problem.
  -- This is for use in examples and tests.
  , withFixedInput) where

import           Solutions.P40  (goldbach)
import qualified Solutions.P74  as Solution
import           System.IO
import           System.Process (createPipe)

{- |
We would like to implement a function which reads an even number from standard input,
finds two prime numbers which add up to the number (see 'Problems.P40.goldbach'),
and prints out the equation to standard output.

Given the use of input and ouput, this could be written with the IO monad in do notation:

@
askGoldbach :: Handle -> Handle -> IO ()
askGoldbach hIn hOut = do
  s <- hGetLine hIn
  let n = read s :: Int
  let (a,b) = goldbach n
  hPutStr hOut $ show n
  hPutStr hOut "="
  hPutStr hOut $ show a
  hPutStr hOut "+"
  hPutStrLn hOut $ show b
@

Implement the function without do notation.
In other words, use '>>=' or '>>' directly, instead of using them implicitly through do notation.
Try to use these functions with prefix style instead of infix style.

=== Examples

>>> withFixedInput "104" stdout askGoldbach
104=3+101

=== __Hint__

In do notation,

> do
>   a
>   b

is a shorthand for

> (>>) a b

or equivalently,

> (>>=) a (\_ -> b)

Also,

> do
>   x <- a
>   b

is a shorthand for

> (>>=) a (\x -> b)

For those not familiar with monads, using these functions directly in prefix style
will make it more apparent that these are not just sequenced statements as
in imperative languages, but really a series of function applications.
The distinction may be minor in the context of IO monads,
but it will make it easier to understand other kinds of monads such as 'Maybe'.

While not relevant to this problem, also note that 'return' is /not/
a return statement as in most languages,
but a function that injects a value into the monad.  For example,

> f :: SomeMonad (Int,Int)
> f = do
>   (a,b) <- return (1,2)
>   return (a+1,b+1)

does not return @(1,2)@, but instead returns a monad with @(2,3)@.

The naming of 'return' can make the function body similar
to what one would expect from imperative languages.  For example,

> f :: IO String
> f = do
>   s <- getLine
>   return $ "Got string: " ++ s

But one should be careful to remember that it is not a return statement as such.
-}
askGoldbach :: Handle -> Handle -> IO ()
askGoldbach = Solution.askGoldbach

{- |
Reads an even number from standard input,
finds two prime numbers which add up to the number,
and prints out the equation to standard output.

This is an implementation of 'askGoldbach' in do notation.

=== Examples

>>> withFixedInput "104" stdout askGoldbach'
104=3+101
-}
askGoldbach' :: Handle -> Handle -> IO ()
askGoldbach' hIn hOut = do
  s <- hGetLine hIn
  let n = read s :: Int
  let (a,b) = goldbach n
  hPutStr hOut $ show n
  hPutStr hOut "="
  hPutStr hOut $ show a
  hPutStr hOut "+"
  hPutStrLn hOut $ show b

{- |
Given a string and an output handle,
apply the function to a constructed input handle and the given output handle.
The input handle will read the given string.

=== Examples

>>> withFixedInput "This is the input." stdout (\i -> \o -> hGetLine i >>= hPutStrLn o)
This is the input.
-}
withFixedInput :: String -> Handle -> (Handle -> Handle -> IO ()) -> IO ()
withFixedInput s hOut f = do
  (readEnd, writeEnd) <- createPipe
  hPutStrLn writeEnd s
  hClose writeEnd
  f readEnd hOut
  hClose readEnd
