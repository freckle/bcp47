{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.BCP47.Trie.Internal
  ( Trie(..)
  , fromList
  , singleton
  , union
  , unionWith
  , unionUsing
  , Trie2(..)
  , Subtags(..)
  , singleton2
  , lookup2
  , match2
  , union2
  , union2Using
  , fromSubtags
  )
  where

import Control.Applicative (liftA2, (<|>))
import Data.BCP47
import Data.BCP47.Internal.Subtags
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Last(Last, getLast))
import Test.QuickCheck.Arbitrary

-- | A trie mapping 'BCP47' tags to values
newtype Trie a
  = Trie { unLanguage :: Map ISO639_1 (Trie2 a)}
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Semigroup a => Semigroup (Trie a) where
  x <> y = unionUsing (liftA2 (<>)) x y

instance Semigroup a => Monoid (Trie a) where
  mempty = Trie mempty

instance Arbitrary a => Arbitrary (Trie a) where
  arbitrary = fromList <$> arbitrary

-- | Construct a 'Trie' from a list of tag/value pairs.
fromList :: [(BCP47, a)] -> Trie a
fromList = foldr (union . uncurry singleton) (Trie mempty)

-- | Construct a 'Trie' from a single tag/value pair.
singleton :: BCP47 -> a -> Trie a
singleton tag = Trie . Map.singleton (language tag) . singleton2 tag

-- | A left-biased union of two 'Trie' structures. The left value is prefered
-- when duplicate tags are found.
union :: Trie a -> Trie a -> Trie a
union = unionUsing (<|>)

-- | 'union' with a combining function.
unionWith :: (a -> a -> a) -> Trie a -> Trie a -> Trie a
unionWith f = unionUsing (liftA2 f)

unionUsing :: (Maybe a -> Maybe a -> Maybe a) -> Trie a -> Trie a -> Trie a
unionUsing f (Trie x) (Trie y) = Trie $ Map.unionWith (union2Using f) x y

data Trie2 a = Trie2 (Maybe a) (Map Subtags (Trie2 a))
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Semigroup a => Semigroup (Trie2 a) where
  x <> y = union2Using (liftA2 (<>)) x y

instance Monoid a => Monoid (Trie2 a) where
  mempty = Trie2 mempty mempty

singleton2 :: BCP47 -> a -> Trie2 a
singleton2 tag = fromSubtags (toSubtags tag)

fromSubtags :: [Subtags] -> a -> Trie2 a
fromSubtags =
  foldr (\path leaf -> Trie2 Nothing . Map.singleton path . leaf) toVal

toVal :: a -> Trie2 a
toVal x = Trie2 (Just x) mempty

lookup2 :: BCP47 -> Trie2 a -> Maybe a
lookup2 tag = getLast . go (toSubtags tag)
 where
  go :: [Subtags] -> Trie2 a -> Last a
  go [] (Trie2 mVal _) = Last mVal
  go (p : ps) (Trie2 mVal children) =
    Last mVal <> (go ps =<< (Last $ Map.lookup p children))

match2 :: BCP47 -> Trie2 a -> Maybe a
match2 tag = go (toSubtags tag)
 where
  go :: [Subtags] -> Trie2 a -> Maybe a
  go [] (Trie2 mVal _) = mVal
  go (p : ps) (Trie2 _ children) = go ps =<< Map.lookup p children

union2 :: Trie2 a -> Trie2 a -> Trie2 a
union2 = union2Using (<|>)

union2Using :: (Maybe a -> Maybe a -> Maybe a) -> Trie2 a -> Trie2 a -> Trie2 a
union2Using f (Trie2 x xs) (Trie2 y ys) =
  Trie2 (f x y) (Map.unionWith union2 xs ys)
