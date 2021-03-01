{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- | Supporting definitions for list problems.
module Problems.Lists (NestedList (Elem, List), Encoding (Single, Multiple)) where

import           Control.DeepSeq
import           GHC.Generics    (Generic)

-- | A list type with arbitrary nesting of lists.
data NestedList a
  = Elem a              -- ^ A non-list element.
  | List [NestedList a] -- ^ Nested list.
  deriving (Eq, Show, Generic)

-- | Encodes one or more consecutively duplicate elements.
data Encoding a
  = Single a       -- ^ Represents a single occurrence of an element.
  | Multiple Int a -- ^ Represents an element repeating consecutively a given number of times.
  deriving (Eq, Show, Generic, NFData)
