{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BCP47.Persist () where

import Control.Monad ((<=<))
import Data.BCP47 (BCP47, fromText, toText)
import Database.Persist.Class (PersistField(..))

instance PersistField BCP47 where
  toPersistValue = toPersistValue . toText
  fromPersistValue = fromText <=< fromPersistValue
