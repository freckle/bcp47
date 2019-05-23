{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BCP47.PathPieces () where

import Data.BCP47 (BCP47, fromText, toText)
import Web.PathPieces (PathPiece(..))

instance PathPiece BCP47 where
  fromPathPiece = either (const Nothing) Just . fromText
  toPathPiece = toText
