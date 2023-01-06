{- |
Description: IO monad without do notation
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P74" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P74 (askGoldbach) where

import           Solutions.P40 (goldbach)
import           System.IO

-- | Implementation of 'Problems.P74.askGoldbach'' without do notation.
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
