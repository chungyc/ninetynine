{- |
Description: Postfix notation
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P79".
-}
module Problems.P79
  ( calculatePostfix
  , Element (..)
  , Operator (..)
    -- * Supporting function
    -- | The function below is not part of the problem.
  , parsePostfix
  ) where

import           Problems.Monads (Element (..), Operator (..), parsePostfix)
import qualified Solutions.P79   as Solution

{- | Postfix notation, also known as reverse Polish notation,
has operators come after their operands in mathematical expressions.
It has no need for operator precedence or parentheses to specify evaluation order.

Evaluation is typically done using a stack.  Numbers are pushed onto the stack,
and operators pop out numbers and pushes back the result.
The t'Control.Monad.State.Lazy.State' monad would be useful for maintaining such a stack.

There may be errors with some expressions.  For example, an expression may be ill-formed,
or there may be a division by zero.  It would be useful to use the 'Maybe' monad so
that we can return 'Nothing' if there is an error.

Finally for this problem, we would like to keep track of changes to the stack and
which operators are applied to which numbers.
The function should also return a list, with each entry showing the state
of the stack after an operand has been pushed or an operator has been applied.
Logging each entry can be done with the t'Control.Monad.Writer.Lazy.Writer' monad.

Unfortunately, it would be very cumbersome to use these monads directly together.
Monad transformers are a way to make it substantially easier to use more than one monad
at the same time.  Use monad transformers to compose the t'Control.Monad.State.Lazy.State',
'Maybe', and t'Control.Monad.Writer.Lazy.Writer' monads into a single monad to implement
a function which evaluates an expression in postfix notation.  It should also
return the history of the calculation.

=== Examples

The result of applying subtraction to 8 and 5 should be 3:

>>> fst $ calculatePostfix [Operand 8, Operand 5, Operator Subtract]
Just 3

It should be an error if no operator is applied to two or more numbers:

>>> fst $ calculatePostfix [Operand 8, Operand 6]
Nothing

Negation applies to a single number:

>>> fst $ calculatePostfix [Operand 8, Operator Negate]
Just (-8)

But it is an error if there is only one number for a binary operator:

>>> fst $ calculatePostfix [Operand 8, Operator Add]
Nothing

The 'parsePostfix' function can be used to conveniently create an expression:

>>> fst $ calculatePostfix $ parsePostfix "8 5 4 10 + - 3 * negate +"
Just 35

The second element in the return value should show the history of the calculation:

>>> mapM_ print $ snd $ calculatePostfix $ parsePostfix "8 5 4 10 + - 3 * negate +"
([8],Nothing)
([5,8],Nothing)
([4,5,8],Nothing)
([10,4,5,8],Nothing)
([14,5,8],Just Add)
([-9,8],Just Subtract)
([3,-9,8],Nothing)
([-27,8],Just Multiply)
([27,8],Just Negate)
([35],Just Add)

Even if the expression is invalid, the second element should
still show the history of the calculation until the point at which there is an error:

>>> fst $ calculatePostfix $ parsePostfix "1 2 * +"
Nothing
>>> mapM_ print $ snd $ calculatePostfix $ parsePostfix "1 2 * +"
([1],Nothing)
([2,1],Nothing)
([2],Just Multiply)

=== __Hint__

The monad transformers for the t'Control.Monad.State.Lazy.State',
'Maybe', and t'Control.Monad.Writer.Lazy.Writer' monads
are t'Control.Monad.State.Lazy.StateT', t'Control.Monad.Trans.Maybe.MaybeT',
and t'Control.Monad.Writer.Lazy.WriterT', respectively.
-}
calculatePostfix :: [Element] -> (Maybe Integer, [([Integer], Maybe Operator)])
calculatePostfix = Solution.calculatePostfix
