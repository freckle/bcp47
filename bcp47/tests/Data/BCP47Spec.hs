{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Data.BCP47Spec
  ( spec
  ) where

import Country.Identifier (china)
import Data.BCP47
import Data.BCP47.Internal.Extension
import Data.BCP47.Internal.LanguageExtension
import Data.BCP47.Internal.PrivateUse
import Data.BCP47.Internal.Script
import Data.BCP47.Internal.Variant
import Data.LanguageCodes (ISO639_1(ZH))
import qualified Data.Set as Set
import Data.Text (unpack)
import Test.Hspec
import Test.QuickCheck (property)
import Text.Read (readMaybe)

spec :: Spec
spec = do
  describe "fromText" $ do
    it "parses all components" $ do
      lng <- either (ioError . userError . unpack) pure
        $ fromText "zh-abc-def-zxy-Hant-CN-1967-y-extensi-x-private1-private2"
      language lng `shouldBe` ZH
      extendedLanguageSubtags lng
        `shouldBe` Set.singleton (LanguageExtension "abc-def-zxy")
      script lng `shouldBe` Just (Script "Hant")
      region lng `shouldBe` Just china
      variants lng `shouldBe` Set.singleton (Variant "1967")
      extensions lng `shouldBe` Set.singleton (Extension "y-extensi")
      privateUse lng
        `shouldBe` Set.fromList [PrivateUse "private1", PrivateUse "private2"]

    it "only parses complete subtags" $ do
      -- Specifically, the region CN should not be parsed out of the variant CNUVWXYX
      lng <- either (ioError . userError . unpack) pure $ fromText "zh-CNUVWXYX"
      language lng `shouldBe` ZH
      extendedLanguageSubtags lng `shouldBe` Set.empty
      script lng `shouldBe` Nothing
      region lng `shouldBe` Nothing
      variants lng `shouldBe` Set.singleton (Variant "CNUVWXYX")
      privateUse lng `shouldBe` Set.empty

  describe "Arbitrary"
    . it "can parse arbitrary generated tags"
    . property
    $ \tag -> fromText (toText tag) `shouldBe` Right tag

  describe "Read/Show" . it "can roundtrip" . property $ \tag ->
    readMaybe (show @BCP47 tag) `shouldBe` Just tag
