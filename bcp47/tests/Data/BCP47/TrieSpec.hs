{-# LANGUAGE OverloadedLists #-}

module Data.BCP47.TrieSpec
  ( spec
  ) where

import Prelude hiding (lookup)

import Data.BCP47
import Data.BCP47.Trie
import qualified Data.List.NonEmpty as NE
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Modifiers (NonEmptyList(NonEmpty))

spec :: Spec
spec = do
  describe "Trie" $ do
    it "has equality" $ property $ \(NonEmpty xs) ->
      fromNonEmpty (NE.fromList xs)
        `shouldBe` (fromNonEmpty (NE.fromList xs) :: Trie Bool)

    it "can be ordered"
      $ singleton en "color"
      < singleton es "color"
      `shouldBe` True

  describe "lookup" $ do
    it "should always lookup a path it inserts" $ property $ \tag ->
      lookup tag (singleton tag "string") `shouldBe` Just "string"

    it "lookups no match" $ do
      let trie = fromNonEmpty [(en, "color"), (enGB, "colour")]
      lookup es trie `shouldBe` Nothing

    it "lookups no match deeply" $ do
      let trie = fromNonEmpty [(enGBTJP, "colour")]
      lookup enGB trie `shouldBe` Nothing

    it "lookups an exact match" $ do
      let trie = fromNonEmpty [(en, "color"), (enGB, "colour")]
      lookup en trie `shouldBe` Just "color"

    it "lookups on just language" $ do
      let trie = fromNonEmpty [(en, "color"), (es, "colour")]
      lookup es trie `shouldBe` Just "colour"

    it "lookups a deep exact match" $ do
      let trie = fromNonEmpty [(enGBTJP, "foo"), (enGB, "colour")]
      lookup enGBTJP trie `shouldBe` Just "foo"

    it "lookups a relevant match" $ do
      let trie = fromNonEmpty [(en, "color"), (enGB, "colour")]
      lookup enTJP trie `shouldBe` Just "color"

    it "lookups a deep relevant match" $ do
      let trie = fromNonEmpty [(en, "color"), (enGB, "colour")]
      lookup enGBTJP trie `shouldBe` Just "colour"

  describe "match" $ do
    it "should always match a path it inserts" $ property $ \tag ->
      match tag (singleton tag "string") `shouldBe` Just "string"

    it "matches no match" $ do
      let trie = fromNonEmpty [(en, "color"), (enGB, "colour")]
      match es trie `shouldBe` Nothing

    it "matches no match deeply" $ do
      let trie = fromNonEmpty [(enGBTJP, "colour")]
      match enGB trie `shouldBe` Nothing

    it "matches an exact match" $ do
      let trie = fromNonEmpty [(en, "color"), (enGB, "colour")]
      match en trie `shouldBe` Just "color"

    it "matches on just language" $ do
      let trie = fromNonEmpty [(en, "color"), (es, "colour")]
      match es trie `shouldBe` Just "colour"

    it "matches a deep exact match" $ do
      let trie = fromNonEmpty [(enGBTJP, "foo"), (enGB, "colour")]
      match enGBTJP trie `shouldBe` Just "foo"

    it "matches a relevant match" $ do
      let trie = fromNonEmpty [(en, "color"), (enGB, "colour")]
      match enTJP trie `shouldBe` Nothing

    it "matches a deep relevant match" $ do
      let trie = fromNonEmpty [(en, "color"), (enGB, "colour")]
      match enGBTJP trie `shouldBe` Nothing
