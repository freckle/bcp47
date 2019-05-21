{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BCP47.Esqueleto () where

import Data.BCP47
import Data.BCP47.Persist ()
import Database.Esqueleto (SqlString)

instance SqlString BCP47
