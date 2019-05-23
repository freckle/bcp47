{-# LANGUAGE TypeApplications #-}

module Data.BCP47.PathPiecesSpec (spec) where

import Data.BCP47 (BCP47)
import Data.BCP47.PathPieces ()
import Test.Hspec
import Test.QuickCheck (property)
import Web.PathPieces (fromPathPiece, toPathPiece)

spec :: Spec
spec = describe "PathPiece" . it "can roundtrip" . property $ \tag ->
  fromPathPiece (toPathPiece @BCP47 tag) == Just tag
