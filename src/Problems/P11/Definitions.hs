module Problems.P11.Definitions (Encoding (Single, Multiple)) where

data Encoding a = Single a | Multiple Int a
  deriving (Eq, Show)
