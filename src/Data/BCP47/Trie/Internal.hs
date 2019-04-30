{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Data.BCP47.Trie.Internal where

import Control.Applicative ((<|>))
import Data.BCP47
import Data.Foldable (toList)
import Data.ISO3166_CountryCodes (CountryCode)
import Data.LanguageCodes (ISO639_1)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Last(Last, getLast))
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Gen (oneof)

-- | Trie
--
-- A BCP47 Trie that allows compact mapping and searching between values and tags.
--
newtype Trie a
  = Trie { unLanguage :: Map ISO639_1 (Trie2 a)}
  deriving (Show, Eq, Ord)

data Trie2 a = Trie2 (Maybe a) (Map Path (Trie2 a))
  deriving (Show, Eq, Ord)

data Path
  = TrieExtendedLanguageSubtag LanguageExtension
  | TrieScript Script
  | TrieRegion CountryCode
  | TrieVariant Variant
  | TrieExtension Extension
  | TriePrivateUse PrivateUse
  deriving (Show, Eq, Ord, Generic)

instance Arbitrary Path where
  arbitrary = oneof
    [ TrieExtendedLanguageSubtag <$> arbitrary
    , TrieScript <$> arbitrary
    , TrieRegion <$> genericArbitrary
    , TrieVariant <$> arbitrary
    , TrieExtension <$> arbitrary
    , TriePrivateUse <$> arbitrary
    ]

singleton2 :: BCP47 -> a -> Trie2 a
singleton2 tag = fromPath (toPath tag)

fromPath :: [Path] -> a -> Trie2 a
fromPath =
  foldr (\path leaf -> Trie2 Nothing . Map.singleton path . leaf) toVal

toPath :: BCP47 -> [Path]
toPath BCP47 {..} =
  (TrieExtendedLanguageSubtag <$> toList extendedLanguageSubtags)
    <> maybe [] (pure . TrieScript) script
    <> maybe [] (pure . TrieRegion) region
    <> (TrieVariant <$> toList variants)
    <> (TrieExtension <$> toList extensions)
    <> (TriePrivateUse <$> toList privateUse)

toVal :: a -> Trie2 a
toVal x = Trie2 (Just x) mempty

find2 :: BCP47 -> Trie2 a -> Maybe a
find2 tag = getLast . go (toPath tag)
 where
  go :: [Path] -> Trie2 a -> Last a
  go [] (Trie2 mVal _) = Last mVal
  go (p : ps) (Trie2 mVal children) =
    Last mVal <> (go ps =<< (Last $ Map.lookup p children))

union2 :: Trie2 a -> Trie2 a -> Trie2 a
union2 (Trie2 x xs) (Trie2 y ys) = Trie2 (x <|> y) (Map.unionWith union2 xs ys)
