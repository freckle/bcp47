-- | A trie like data structure for defining maps from 'BCP47' tags to values.
--
-- This structure supports collection and lookup of language tagged values. Its
-- semantics are based on those defined in the BCP 47 specification.
--
module Data.BCP47.Trie
  ( Trie
  , fromList
  , singleton
  , lookup
  , match
  , elem
  , union
  , unionWith
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

-- | Check if a tag exists in the 'Trie'
elem :: BCP47 -> Trie a -> Bool
elem tag = isJust . match tag
