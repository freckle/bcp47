{-# LANGUAGE TypeApplications #-}

module Data.BCP47.CsvSpec (spec) where

import Data.BCP47 (BCP47)
import Data.BCP47.Csv ()
import Data.Csv (parseField, runParser, toField)
import Test.Hspec
import Test.QuickCheck (property)

spec :: Spec
spec = describe "ToField/FromField" . it "can roundtrip" . property $ \tag ->
  runParser (parseField $ toField @BCP47 tag) == Right tag
