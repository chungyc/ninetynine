{-# LANGUAGE DeriveGeneric #-}
-- | Definitions used by Ninety-Nine Haskell "Problems".
module Problems.P07.Definitions (NestedList (Elem, List)) where

import           GHC.Generics (Generic)

-- | A list type with arbitrary nesting of lists.
data NestedList a
  = Elem a              -- ^ A non-list element.
  | List [NestedList a] -- ^ Nested list.
  deriving (Eq, Show, Generic)
