{-# LANGUAGE TypeApplications #-}

module Data.BCP47.AesonSpec (spec) where

import Data.Aeson (decode, encode)
import Data.BCP47 (BCP47)
import Data.BCP47.Aeson ()
import Test.Hspec
import Test.QuickCheck (property)

spec :: Spec
spec = describe "ToJSON/FromJSON" . it "can roundtrip" . property $ \tag ->
  (decode $ encode @BCP47 tag) == Just tag
