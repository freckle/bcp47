{-# LANGUAGE RecordWildCards #-}

module Data.BCP47.Trie
  ( Trie
  , fromList
  , singleton
  , find
  , union
  )
where

import Data.BCP47
import Data.BCP47.Trie.Internal
import qualified Data.Map as Map

fromList :: [(BCP47, a)] -> Trie a
fromList = foldr (union . uncurry singleton) (Trie mempty)

singleton :: BCP47 -> a -> Trie a
singleton tag@BCP47 {..} = Trie . Map.singleton language . singleton2 tag

find :: BCP47 -> Trie a -> Maybe a
find tag trie = find2 tag =<< Map.lookup (language tag) (unLanguage trie)

union :: Trie a -> Trie a -> Trie a
union (Trie x) (Trie y) = Trie $ Map.unionWith union2 x y
