{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Problems.P11.Definitions (Encoding (Single, Multiple)) where

import           Control.DeepSeq
import           GHC.Generics    (Generic)

data Encoding a = Single a | Multiple Int a
  deriving (Eq, Show, Generic, NFData)
