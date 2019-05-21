module Data.BCP47.Trie
  ( Trie
  , fromList
  , singleton
  , lookup
  , match
  , elem
  , union
  , unionWith
  , null
  )
where

import Prelude hiding (elem, lookup, null)

import Data.BCP47
import Data.BCP47.Trie.Internal
import qualified Data.Map as Map
import Data.Maybe (isJust)

-- | Lookup the most relevant item for a tag
lookup :: BCP47 -> Trie a -> Maybe a
lookup tag trie = lookup2 tag =<< Map.lookup (language tag) (unLanguage trie)

-- | Lookup an exact match for a tag
match :: BCP47 -> Trie a -> Maybe a
match tag trie = match2 tag =<< Map.lookup (language tag) (unLanguage trie)

-- | Check if a tag exists in the Trie
elem :: BCP47 -> Trie a -> Bool
elem tag = isJust . match tag

-- | Check if a Trie is empty
null :: Trie a -> Bool
null = Map.null . unLanguage
