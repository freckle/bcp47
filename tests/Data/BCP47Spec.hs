{-# LANGUAGE OverloadedStrings #-}

module Data.BCP47Spec (spec) where

import Data.BCP47
import Data.BCP47.Internal.Extension
import Data.BCP47.Internal.LanguageExtension
import Data.BCP47.Internal.PrivateUse
import Data.BCP47.Internal.Script
import Data.BCP47.Internal.Variant
import Data.ISO3166_CountryCodes (CountryCode(CN))
import Data.LanguageCodes (ISO639_1(ZH))
import qualified Data.Set as Set
import Data.Text (unpack)
import Test.Hspec

spec :: Spec
spec = describe "fromText" $ it "parses all components" $ do
  lng <- either (ioError . userError . unpack) pure
    $ fromText "zh-abc-def-zxy-Hant-CN-1967-y-extensi-x-private1-private2"
  language lng `shouldBe` ZH
  extendedLanguageSubtags lng
    `shouldBe` Set.singleton (LanguageExtension "abc-def-zxy")
  script lng `shouldBe` Just (Script "Hant")
  region lng `shouldBe` Just CN
  variants lng `shouldBe` Set.singleton (Variant "1967")
  extensions lng `shouldBe` Set.singleton (Extension "y-extensi")
  privateUse lng
    `shouldBe` Set.fromList [PrivateUse "private1", PrivateUse "private2"]
