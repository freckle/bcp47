{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BCP47.OpenApi () where

import Autodocodec (Autodocodec (..))
import Autodocodec.OpenAPI.DerivingVia ()
import Data.BCP47
import Data.BCP47.Autodocodec ()
import Data.OpenApi.ParamSchema (ToParamSchema (..))
import Data.OpenApi.Schema (ToSchema (..))
import Data.Proxy (Proxy (..))

deriving via (Autodocodec BCP47) instance ToSchema BCP47

instance ToParamSchema BCP47 where
  toParamSchema _ = toParamSchema (Proxy :: Proxy String)
