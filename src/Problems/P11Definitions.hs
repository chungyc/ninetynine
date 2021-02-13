module Problems.P11Definitions (Encoding (Single, Multiple)) where

data Encoding a = Single a | Multiple Int a
  deriving (Eq, Show)
