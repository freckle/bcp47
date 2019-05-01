module Data.BCP47.Trie
  ( Trie
  , fromList
  , singleton
  , lookup
  , union
  , unionWith
  )
where

import Prelude hiding (lookup)

import Data.BCP47
import Data.BCP47.Trie.Internal
import qualified Data.Map as Map

-- | Lookup the most relevant item for a tag
lookup :: BCP47 -> Trie a -> Maybe a
lookup tag trie = lookup2 tag =<< Map.lookup (language tag) (unLanguage trie)
