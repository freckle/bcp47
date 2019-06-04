{-# LANGUAGE DeriveGeneric #-}

module Data.BCP47.Internal.Subtags
  ( Subtags(..)
  ) where

import Data.BCP47.Internal.Extension
import Data.BCP47.Internal.LanguageExtension
import Data.BCP47.Internal.PrivateUse
import Data.BCP47.Internal.Region
import Data.BCP47.Internal.Script
import Data.BCP47.Internal.Variant
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Gen (oneof)

data Subtags
  = SpecifyLanguageExtension LanguageExtension
  | SpecifyScript Script
  | SpecifyRegion CountryCode
  | SpecifyVariant Variant
  | SpecifyExtension Extension
  | SpecifyPrivateUse PrivateUse
  deriving (Show, Eq, Ord, Generic)

instance Arbitrary Subtags where
  arbitrary = oneof
    [ SpecifyLanguageExtension <$> arbitrary
    , SpecifyScript <$> arbitrary
    , SpecifyRegion <$> genericArbitrary
    , SpecifyVariant <$> arbitrary
    , SpecifyExtension <$> arbitrary
    , SpecifyPrivateUse <$> arbitrary
    ]


