

module Data.BCP47.Internal.Region
  ( CountryCode
  , regionToText
  , regionFromText
  , regionP
  )
where

import Data.Bifunctor (first)
import Data.ISO3166_CountryCodes (CountryCode)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (Parsec, count, parse)
import Text.Megaparsec.Char (upperChar)
import Text.Megaparsec.Error (parseErrorPretty)
import Text.Read (readEither)

regionToText :: CountryCode -> Text
regionToText = pack . show


regionFromText :: Text -> Either Text CountryCode
regionFromText =
  first (pack . parseErrorPretty) . parse regionP "regionFromText"

-- | BCP-47 region parser
--
-- This only implements the ISO portion of the grammar.
--
-- @@
-- region        = 2ALPHA              ; ISO 3166-1 code
--               / 3DIGIT              ; UN M.49 code
-- @@
--
regionP :: Parsec Void Text CountryCode
regionP = either fail pure . readEither =<< count 2 upperChar
