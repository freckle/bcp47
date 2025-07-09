{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BCP47.OpenApi () where

import Control.Lens ((&), (?~))
import Data.Aeson (Value (..))
import Data.BCP47
import Data.BCP47.Autodocodec ()
import Data.OpenApi (description, example, format, schema)
import Data.OpenApi.ParamSchema (ToParamSchema (..))
import Data.OpenApi.Schema (ToSchema (..), toSchema)
import Data.Proxy (Proxy (..))
import Data.Text (Text)

instance ToSchema BCP47 where
  declareNamedSchema _ = do
    -- Use the schema for Text, but add more details
    textSchema <- declareNamedSchema $ Proxy @Text

    pure
      $ textSchema
        & schema . description ?~ "BCP47 language tag"
        & schema . example ?~ String "en"
        & schema . format ?~ "bcp47"

instance ToParamSchema BCP47 where
  toParamSchema _ = toSchema $ Proxy @BCP47
