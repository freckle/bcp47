{-# LANGUAGE OverloadedStrings #-}

module Data.BCP47.Internal.LanguageExtension
  ( LanguageExtension(LanguageExtension)
  , languageExtensionFromText
  , languageExtensionToText
  , languageExtensionP
  )
where

import Data.Bifunctor (first)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (Parsec, count, parse)
import Text.Megaparsec.Char (letterChar)
import Text.Megaparsec.Error (parseErrorPretty)

newtype LanguageExtension = LanguageExtension { languageExtensionToText :: Text }
  deriving (Show, Eq, Ord)

languageExtensionFromText :: Text -> Either Text LanguageExtension
languageExtensionFromText = first (pack . parseErrorPretty)
  . parse languageExtensionP "languageExtensionFromText"

-- | BCP-47 language extension parser
--
-- This only implements the ISO 639 portion of the ISO.
--
-- @@
--  extlang       = 3ALPHA              ; selected ISO 639 codes
--                 *2("-" 3ALPHA)      ; permanently reserved
-- @@
--
-- FIXME this is wrong
languageExtensionP :: Parsec Void Text LanguageExtension
languageExtensionP = LanguageExtension . pack <$> count 3 letterChar
