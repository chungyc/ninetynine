{- |
Description: Maybe monads
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P75" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P75 (maybeGoldbach) where

import           Control.Monad (guard)
import           Solutions.P40 (goldbach)
import           Text.Read     (readMaybe)

-- $setup
-- >>> import Problems.P74  (withFixedInput)
-- >>> import System.IO

{- |
In "Problems.P75", 'Problems.P75.askGoldbach' could not output an error if the input was not a number
or it was not an even number greater than 2.  We could implement a function which returned 'Nothing'
when the input is not valid, such as in the following.

@
maybeGoldbach :: String -> Maybe (Int, (Int,Int))
maybeGoldbach s =
  case readMaybe s of
    Nothing -> Nothing
    Just n -> if n < 2 then Nothing else
                if odd n then Nothing else
                  Just (n, goldbach n)
@

Then we could take advantage of this function to print out an error if the input is not valid.

>>> :{
let format (n, (a,b)) = (show n) ++ "=" ++ (show a) ++ "+" ++ (show b)
    maybeAskGoldbach hIn hOut = do
      s <- hGetLine hIn
      case maybeGoldbach s of
        Nothing -> hPutStrLn hOut "error"
        Just x  -> hPutStrLn hOut $ format x
in do
  withFixedInput "104" stdout maybeAskGoldbach
  withFixedInput "not a number" stdout maybeAskGoldbach
:}
104=3+101
error

However, the implementation of @maybeGoldbach@ above is a chain of conditional expressions.
It is not problematic in this particular case, but can make things awkward when there
are many conditions and successful operations that need to happen
for a function to return a 'Maybe' value.

Take advantage of the fact that 'Maybe' is a monad
and rewrite @maybeGoldbach@ more succintly using do notation.
The 'Control.Monad.guard' function, which in a 'Maybe' monad
returns @Just ()@ when its argument is true
and @Nothing@ when its argument is false,
would be useful for making it even more succinct.
-}
maybeGoldbach :: String -> Maybe (Int, (Int,Int))
maybeGoldbach s = do
  n <- readMaybe s
  guard $ n > 2
  guard $ even n
  return $ (n, goldbach n)
