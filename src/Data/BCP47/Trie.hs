{-# LANGUAGE RecordWildCards #-}

module Data.BCP47.Trie
  ( Trie
  , fromList
  , singleton
  , lookup
  , union
  )
where

import Prelude hiding (lookup)

import Data.BCP47
import Data.BCP47.Trie.Internal
import qualified Data.Map as Map

fromList :: [(BCP47, a)] -> Trie a
fromList = foldr (union . uncurry singleton) (Trie mempty)

singleton :: BCP47 -> a -> Trie a
singleton tag@BCP47 {..} = Trie . Map.singleton language . singleton2 tag

-- | Lookup the most relevant item for a tag
lookup :: BCP47 -> Trie a -> Maybe a
lookup tag trie = lookup2 tag =<< Map.lookup (language tag) (unLanguage trie)

union :: Trie a -> Trie a -> Trie a
union (Trie x) (Trie y) = Trie $ Map.unionWith union2 x y
