{-# LANGUAGE DerivingStrategies #-}
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
import Data.BCP47.Internal.Parser (complete, asciiLetter)
import Data.Bifunctor (first)
import Data.CaseInsensitive (CI, mk)
import Data.List (intercalate)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (Parsec, count, parse)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Error (errorBundlePretty)

-- | Extended language subtags
--
-- These are used to identify certain specially selected languages that, for
-- various historical and compatibility reasons, are closely identified with or
-- tagged using an existing primary language subtag.
--
newtype LanguageExtension = LanguageExtension { languageExtensionToText :: CI Text }
  deriving stock (Show, Eq, Ord)

instance Arbitrary LanguageExtension where
  arbitrary = do
    components <- replicateM 3 $ alphaString 3
    pure $ LanguageExtension $ mk $ pack $ intercalate "-" components

-- | Parse a 'LanguageExtension' subtag from 'Text'
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
languageExtensionP = complete $ do
  iso639 <- count 3 asciiLetter
  void $ char '-'
  c1 <- count 3 asciiLetter
  void $ char '-'
  c2 <- count 3 asciiLetter
  let ext = mk $ pack $ mconcat [iso639, "-", c1, "-", c2]
  pure $ LanguageExtension ext
