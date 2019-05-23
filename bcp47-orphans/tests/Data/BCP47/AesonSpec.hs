{-# LANGUAGE TypeApplications #-}

module Data.BCP47.AesonSpec
  ( spec
  ) where

import Data.Aeson (decode, encode)
import Data.BCP47 (BCP47)
import Data.BCP47.Aeson ()
import Data.BCP47.Roundtrip (roundtrips)
import Test.Hspec
import Test.QuickCheck (property)

spec :: Spec
spec =
  describe "ToJSON/FromJSON" . it "roundtrips" . property $ roundtrips @BCP47
    encode
    decode
