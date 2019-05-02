{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Data.BCP47.Trie.Internal
  ( Trie(..)
  , fromList
  , singleton
  , union
  , unionWith
  , unionUsing
  , Trie2(..)
  , Path(..)
  , singleton2
  , lookup2
  , match2
  , union2
  , union2Using
  , fromPath
  , toPath
  )
  where

import Control.Applicative (liftA2, (<|>))
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

instance Semigroup a => Semigroup (Trie a) where
  x <> y = unionUsing (liftA2 (<>)) x y

instance Monoid a => Monoid (Trie a) where
  mempty = Trie mempty

instance Arbitrary a => Arbitrary (Trie a) where
  arbitrary = fromList <$> arbitrary

fromList :: [(BCP47, a)] -> Trie a
fromList = foldr (union . uncurry singleton) (Trie mempty)

singleton :: BCP47 -> a -> Trie a
singleton tag@BCP47 {..} = Trie . Map.singleton language . singleton2 tag

union :: Trie a -> Trie a -> Trie a
union = unionUsing (<|>)

unionWith :: (a -> a -> a) -> Trie a -> Trie a -> Trie a
unionWith f = unionUsing (liftA2 f)

unionUsing :: (Maybe a -> Maybe a -> Maybe a) -> Trie a -> Trie a -> Trie a
unionUsing f (Trie x) (Trie y) = Trie $ Map.unionWith (union2Using f) x y

data Trie2 a = Trie2 (Maybe a) (Map Path (Trie2 a))
  deriving (Show, Eq, Ord)

instance Semigroup a => Semigroup (Trie2 a) where
  x <> y = union2Using (liftA2 (<>)) x y

instance Monoid a => Monoid (Trie2 a) where
  mempty = Trie2 mempty mempty

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

lookup2 :: BCP47 -> Trie2 a -> Maybe a
lookup2 tag = getLast . go (toPath tag)
 where
  go :: [Path] -> Trie2 a -> Last a
  go [] (Trie2 mVal _) = Last mVal
  go (p : ps) (Trie2 mVal children) =
    Last mVal <> (go ps =<< (Last $ Map.lookup p children))

match2 :: BCP47 -> Trie2 a -> Maybe a
match2 tag = go (toPath tag)
 where
  go :: [Path] -> Trie2 a -> Maybe a
  go [] (Trie2 mVal _) = mVal
  go (p : ps) (Trie2 _ children) = go ps =<< Map.lookup p children

union2 :: Trie2 a -> Trie2 a -> Trie2 a
union2 = union2Using (<|>)

union2Using :: (Maybe a -> Maybe a -> Maybe a) -> Trie2 a -> Trie2 a -> Trie2 a
union2Using f (Trie2 x xs) (Trie2 y ys) =
  Trie2 (f x y) (Map.unionWith union2 xs ys)
