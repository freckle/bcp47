{-# LANGUAGE NamedFieldPuns #-}

module Data.BCP47
  ( BCP47(..)
  , mkLanguage
  , mkLocalized
  , fromText
  -- * Components
  , ISO639_1
  , LanguageExtension
  , languageExtensionToText
  , Script
  , scriptToText
  , CountryCode
  , Variant
  , variantToText
  , Extension
  , extensionToText
  , PrivateUse
  , privateUseToText
  )
where

import Control.Applicative ((<|>))
import Control.Monad (void, when)
import Data.Bifunctor (first)
import Data.ISO3166_CountryCodes (CountryCode)
import Data.LanguageCodes (ISO639_1, fromChars)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (Parsec, count, count', eof, hidden, many, optional, parse, try)
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, lowerChar, upperChar)
import Text.Megaparsec.Error (parseErrorPretty)
import Text.Read (readEither)

-- | BCP-47
--
-- https://tools.ietf.org/html/bcp47
--
data BCP47
  = BCP47
  { language :: ISO639_1
  , extendedLanguageSubtags :: [LanguageExtension]
  , script :: Maybe Script
  , region :: Maybe CountryCode
  , variants :: [Variant]
  , extensions :: [Extension]
  , privateUse :: [PrivateUse]
  }
  deriving Show

newtype LanguageExtension = LanguageExtension { languageExtensionToText :: Text }
  deriving Show

newtype Script = Script { scriptToText :: Text }
  deriving Show

newtype Variant = Variant { variantToText :: Text }
  deriving Show

newtype Extension = Extension { extensionToText :: Text }
  deriving Show

newtype PrivateUse = PrivateUse { privateUseToText :: Text }
  deriving Show

mkLanguage :: ISO639_1 -> BCP47
mkLanguage lang = BCP47 lang [] Nothing Nothing [] [] []

mkLocalized :: ISO639_1 -> CountryCode -> BCP47
mkLocalized lang locale = BCP47 lang [] Nothing (Just locale) [] [] []

fromText :: Text -> Either Text BCP47
fromText = first (pack . parseErrorPretty) . parse parser "fromText"

parser :: Parsec Void Text BCP47
parser =
  BCP47
    <$> languageP
    <*> many (try (char '-' *> languageExtP))
    <*> (try (optional $ char '-' *> scriptP) <|> pure Nothing)
    <*> (try (optional (char '-' *> regionP)) <|> pure Nothing)
    <*> many (try (char '-' *> variantP))
    <*> many (try (char '-' *> extensionP))
    <*> many (try (char '-' *> privateUseP))
    <* hidden eof

-- | BCP-47 language parser
--
-- This only implements the ISO 639 portion of the ISO.
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
languageP = do
  x <- lowerChar
  mCode <- fromChars x <$> lowerChar
  maybe (fail "unknown ISO-639-1 code") pure mCode

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
languageExtP :: Parsec Void Text LanguageExtension
languageExtP = LanguageExtension . pack <$> count 3 letterChar

-- | BCP-47 script parser
--
-- @@
--  script        = 4ALPHA              ; ISO 15924 code
-- @@
--
scriptP :: Parsec Void Text Script
scriptP = Script . pack <$> count 4 letterChar

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

-- | BCP-47 variant parser
--
-- @@
-- variant       = 5*8alphanum         ; registered variants
--               / (DIGIT 3alphanum)
-- @@
--
variantP :: Parsec Void Text Variant
variantP = Variant . pack <$> count' 5 8 alphaNumChar

-- | BCP-47 extension parser
--
-- @@
-- extension     = singleton 1*("-" (2*8alphanum))
--                                     ; Single alphanumerics
--                                     ; "x" reserved for private use
--
-- singleton     = DIGIT               ; 0 - 9
--               / %x41-57             ; A - W
--               / %x59-5A             ; Y - Z
--               / %x61-77             ; a - w
--               / %x79-7A             ; y - z
-- @@
--
extensionP :: Parsec Void Text Extension
extensionP = Extension . pack <$> do
  ext <- alphaNumChar
  when (ext `elem` ['x', 'X']) $ fail "private use suffix found"
  void $ char '-'
  rest <- count' 2 8 alphaNumChar
  pure $ ext : '-' : rest

-- | BCP-47 private use parser
--
-- @@
-- privateuse    = "x" 1*("-" (1*8alphanum))
-- @@
--
privateUseP :: Parsec Void Text PrivateUse
privateUseP = PrivateUse . pack <$> do
  ext <- char 'x'
  void $ char '-'
  rest <- count' 1 8 alphaNumChar
  pure $ ext : '-' : rest
