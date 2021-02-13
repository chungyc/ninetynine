{-# LANGUAGE DeriveGeneric #-}
module Problems.P07Definitions (NestedList (Elem, List)) where

import           GHC.Generics (Generic)

-- | A list type with arbitrary nesting of lists.
data NestedList a = Elem a | List [NestedList a]
  deriving (Show, Generic)
