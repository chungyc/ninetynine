{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{- |
Description: 'Encoding'

Definitions used by "Problems.P11".
-}
module Problems.P11.Definitions (Encoding (Single, Multiple)) where

import           Control.DeepSeq
import           GHC.Generics    (Generic)

-- | Encodes one or more consecutively duplicate elements.
data Encoding a
  = Single a       -- ^ Represents a single occurrence of an element.
  | Multiple Int a -- ^ Represents an element repeating consecutively a given number of times.
  deriving (Eq, Show, Generic, NFData)