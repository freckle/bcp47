{-# LANGUAGE OverloadedStrings #-}

module Data.BCP47.Internal.LanguageExtension
  ( LanguageExtension(LanguageExtension)
  , languageExtensionFromText
  , languageExtensionToText
  , languageExtensionP
  )
where

import Control.Monad (replicateM, void)
import Data.BCP47.Internal.Arbitrary (Arbitrary, alphaString, arbitrary)
import Data.Bifunctor (first)
import Data.List (intercalate)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (Parsec, count, parse)
import Text.Megaparsec.Char (char, letterChar)
import Text.Megaparsec.Error (errorBundlePretty)

newtype LanguageExtension = LanguageExtension { languageExtensionToText :: Text }
  deriving (Show, Eq, Ord)

instance Arbitrary LanguageExtension where
  arbitrary = do
    components <- replicateM 3 $ alphaString 3
    pure . LanguageExtension $ pack $ intercalate "-" components

languageExtensionFromText :: Text -> Either Text LanguageExtension
languageExtensionFromText = first (pack . errorBundlePretty)
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
