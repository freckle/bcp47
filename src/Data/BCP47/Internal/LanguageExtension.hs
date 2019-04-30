{-# LANGUAGE OverloadedStrings #-}

module Data.BCP47.Internal.LanguageExtension
  ( LanguageExtension(LanguageExtension)
  , languageExtensionFromText
  , languageExtensionToText
  , languageExtensionP
  )
where

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Text (Text, pack)
import Data.Void (Void)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Text.Megaparsec (Parsec, count, parse)
import Text.Megaparsec.Char (char, letterChar)
import Text.Megaparsec.Error (parseErrorPretty)

newtype LanguageExtension = LanguageExtension { languageExtensionToText :: Text }
  deriving (Show, Eq, Ord)

instance Arbitrary LanguageExtension where
  arbitrary = LanguageExtension . pack <$> arbitrary

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
languageExtensionP :: Parsec Void Text LanguageExtension
languageExtensionP = LanguageExtension . pack <$> do
  iso639 <- count 3 letterChar
  void $ char '-'
  c1 <- count 3 letterChar
  void $ char '-'
  c2 <- count 3 letterChar
  pure $ mconcat [iso639, "-", c1, "-", c2]
