{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BCP47.Serialise () where

import Codec.Serialise
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding
import Control.Monad (void)
import Data.BCP47 (BCP47, toText, fromText)
import Data.Text (unpack)

instance Serialise BCP47 where
  -- bypass children not having their own Serialise instances by using toText/fromText
  encode bcp = encodeListLen 2 <> encodeTag 0 <> encodeString (toText bcp)
  decode = do
    -- these two must be read to consume them 
    -- ignore them as they're here as part of the standard format but we don't need them
    void decodeListLen
    void decodeTag
    textual <- decodeString
    case fromText textual of
      Left err -> fail . unpack $ err
      Right val -> pure val