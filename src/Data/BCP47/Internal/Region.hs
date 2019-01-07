

module Data.BCP47.Internal.Region
  ( CountryCode
  , regionP
  )
where

import Data.ISO3166_CountryCodes (CountryCode)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, count)
import Text.Megaparsec.Char (upperChar)
import Text.Read (readEither)

-- | BCP-47 region parser
--
-- This only implements the ISO portion of the parser.
--
-- @@
-- region        = 2ALPHA              ; ISO 3166-1 code
--               / 3DIGIT              ; UN M.49 code
-- @@
--
regionP :: Parsec Void Text CountryCode
regionP = either fail pure . readEither =<< count 2 upperChar
