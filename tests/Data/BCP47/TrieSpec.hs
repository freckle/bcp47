module Data.BCP47.TrieSpec (spec) where

import Data.BCP47
import Data.BCP47.Trie
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "find" $ do
  it "should always find a path it inserts" $ property $ \tag ->
    find tag (singleton tag "string") `shouldBe` Just "string"
  it "has equality" $ property $ \xs ->
    fromList xs `shouldBe` (fromList xs :: Trie Bool)
  it "can be ordered"
    $ singleton en "color"
    < singleton es "color"
    `shouldBe` True
  it "finds no match" $ do
    let trie = fromList [(en, "color"), (enGB, "colour")]
    find es trie `shouldBe` Nothing
  it "finds no match deeply" $ do
    let trie = fromList [(enGBTJP, "colour")]
    find enGB trie `shouldBe` Nothing
  it "finds an exact match" $ do
    let trie = fromList [(en, "color"), (enGB, "colour")]
    find en trie `shouldBe` Just "color"
  it "finds on just language" $ do
    let trie = fromList [(en, "color"), (es, "colour")]
    find es trie `shouldBe` Just "colour"
  it "finds a deep exact match" $ do
    let trie = fromList [(enGBTJP, "foo"), (enGB, "colour")]
    find enGBTJP trie `shouldBe` Just "foo"
  it "finds a relevant match" $ do
    let trie = fromList [(en, "color"), (enGB, "colour")]
    find enTJP trie `shouldBe` Just "color"
  it "finds a deep relevant match" $ do
    let trie = fromList [(en, "color"), (enGB, "colour")]
    find enGBTJP trie `shouldBe` Just "colour"
