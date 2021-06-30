{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.BCP47.Internal.CIText
  ( CIText(..)
  , fromText
  , pack
  , original
  , foldedCase
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.CaseInsensitive (CI)
import Data.String (IsString)
import qualified Data.CaseInsensitive as CI

-- | Internal newtype for 'CI Text'
--
-- Constructor is exposed since this is an internal module, but ths
-- interface may change. Module is meant to be imported qualified.
--
newtype CIText = CIText
  { unCIText :: CI Text
  }
  deriving newtype (Eq, Show, Ord, IsString)

-- | Convert 'Text' to 'CIText'
fromText :: Text -> CIText
fromText = CIText . CI.mk
{-# INLINE fromText #-}

-- | Convert 'String' to 'CIText'
pack :: String -> CIText
pack = fromText . T.pack
{-# INLINE pack #-}

-- | Extract case-folded 'Text'
foldedCase :: CIText -> Text
foldedCase = CI.foldedCase . unCIText
{-# INLINE foldedCase #-}

-- | Recover original 'Text'
original :: CIText -> Text
original = CI.original . unCIText
{-# INLINE original #-}
