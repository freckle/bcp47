module Data.BCP47.TrieSpec (spec) where

import Prelude hiding (lookup)

import Data.BCP47
import Data.BCP47.Trie
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "lookup" $ do
  it "should always lookup a path it inserts" $ property $ \tag ->
    lookup tag (singleton tag "string") `shouldBe` Just "string"

  it "has equality" $ property $ \xs ->
    fromList xs `shouldBe` (fromList xs :: Trie Bool)

  it "can be ordered"
    $ singleton en "color"
    < singleton es "color"
    `shouldBe` True

  it "lookups no match" $ do
    let trie = fromList [(en, "color"), (enGB, "colour")]
    lookup es trie `shouldBe` Nothing

  it "lookups no match deeply" $ do
    let trie = fromList [(enGBTJP, "colour")]
    lookup enGB trie `shouldBe` Nothing

  it "lookups an exact match" $ do
    let trie = fromList [(en, "color"), (enGB, "colour")]
    lookup en trie `shouldBe` Just "color"

  it "lookups on just language" $ do
    let trie = fromList [(en, "color"), (es, "colour")]
    lookup es trie `shouldBe` Just "colour"

  it "lookups a deep exact match" $ do
    let trie = fromList [(enGBTJP, "foo"), (enGB, "colour")]
    lookup enGBTJP trie `shouldBe` Just "foo"

  it "lookups a relevant match" $ do
    let trie = fromList [(en, "color"), (enGB, "colour")]
    lookup enTJP trie `shouldBe` Just "color"

  it "lookups a deep relevant match" $ do
    let trie = fromList [(en, "color"), (enGB, "colour")]
    lookup enGBTJP trie `shouldBe` Just "colour"
