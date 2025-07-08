{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BCP47.Aeson () where

import Data.Aeson
import Data.BCP47
import Data.Text (unpack)

instance ToJSON BCP47 where
  toEncoding = toEncoding . toText
  toJSON = toJSON . toText

instance FromJSON BCP47 where
  parseJSON = withText "BCP47" $ either (fail . unpack) pure . fromText
