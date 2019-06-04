{-# LANGUAGE OverloadedStrings #-}

module Data.BCP47.Internal.Language
  ( ISO639_1
  , languageFromText
  , languageToText
  , languageP
  )
where

import Data.BCP47.Internal.Parser (complete)
import Data.Bifunctor (first)
import Data.LanguageCodes (ISO639_1, fromChars)
import Data.Text (Text, pack, toLower)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parse)
import Text.Megaparsec.Char (lowerChar)
import Text.Megaparsec.Error (errorBundlePretty)

-- | Parse a language subtag from 'Text'
languageToText :: ISO639_1 -> Text
languageToText = toLower . pack . show

languageFromText :: Text -> Either Text ISO639_1
languageFromText =
  first (pack . errorBundlePretty) . parse languageP "languageFromText"

-- | BCP-47 language parser
--
-- This only implements the ISO 639 portion of the grammar.
--
-- @@
--  language      = 2*3ALPHA            ; shortest ISO 639 code
--                  ["-" extlang]       ; sometimes followed by
--                                      ; extended language subtags
--                / 4ALPHA              ; or reserved for future use
--                / 5*8ALPHA            ; or registered language subtag
-- @@
--
languageP :: Parsec Void Text ISO639_1
languageP = complete $ do
  mCode <- fromChars <$> lowerChar <*> lowerChar
  maybe (fail "unknown ISO-639-1 code") pure mCode
