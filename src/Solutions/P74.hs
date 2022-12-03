{- |
Description: IO monad without do notation
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P74" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P74 (askGoldbach) where

import           Problems.P40 (goldbach)
import           System.IO

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
-}
askGoldbach :: Handle -> Handle -> IO ()
askGoldbach hIn hOut =
  (>>=) (hGetLine hIn)
        (\s -> let n = read s :: Int in
                 let (a,b) = goldbach n in
                   (>>=) (hPutStr hOut $ show n)
                         (\_ -> (>>=) (hPutStr hOut "=")
                                      (\_ -> (>>=) (hPutStr hOut $ show a)
                                                   (\_ -> (>>=) (hPutStr hOut "+")
                                                                (\_ -> (hPutStrLn hOut $ show b))))))
