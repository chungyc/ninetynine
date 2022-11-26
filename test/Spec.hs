{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

module SpecHook where

import           Test.Hspec

hook :: Spec -> Spec
hook = parallel
