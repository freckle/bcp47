{-# LANGUAGE TypeApplications #-}

module Data.BCP47.PersistSpec (spec) where

import Data.BCP47 (BCP47)
import Data.BCP47.Persist ()
import Database.Persist.Class (fromPersistValue, toPersistValue)
import Test.Hspec
import Test.QuickCheck (property)

spec :: Spec
spec = describe "PersistField" . it "can roundtrip" . property $ \tag ->
  fromPersistValue (toPersistValue @BCP47 tag) == Right tag
