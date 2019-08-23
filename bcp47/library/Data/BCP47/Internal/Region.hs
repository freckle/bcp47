module Data.BCP47.Internal.Region
  ( Country
  , regionToText
  , regionFromText
  , regionP
  )
where

import Control.Applicative ((<|>))
import Country (Country, alphaTwoUpper, decodeAlphaTwo, decodeNumeric)
import Data.BCP47.Internal.Parser (complete)
import Data.Bifunctor (first)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (Parsec, count, parse, try)
import Text.Megaparsec.Char (digitChar, upperChar)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Read (readEither)

regionToText :: Country -> Text
regionToText = alphaTwoUpper

-- | Parse a region subtag from 'Text'
regionFromText :: Text -> Either Text Country
regionFromText =
  first (pack . errorBundlePretty) . parse regionP "regionFromText"

-- | BCP-47 region parser
--
-- @@
-- region        = 2ALPHA              ; ISO 3166-1 code
--               / 3DIGIT              ; UN M.49 code
-- @@
--
regionP :: Parsec Void Text Country
regionP = complete $ try alpha2 <|> num3
 where
  alpha2 =
    maybe (fail "Invalid 2 character country code") pure
      . decodeAlphaTwo
      . pack
      =<< count 2 upperChar
  num3 =
    maybe (fail "Invalid 3 character country code") pure
      . decodeNumeric
      =<< either fail pure
      . readEither
      =<< count 3 digitChar
