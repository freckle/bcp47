{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.BCP47.Internal.Script
  ( Script(Script)
  , scriptFromText
  , scriptToText
  , scriptP
  )
where

import Data.BCP47.Internal.Arbitrary (Arbitrary, alphaString, arbitrary)
import Data.BCP47.Internal.Parser (complete)
import Data.Bifunctor (first)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (Parsec, count, parse)
import Text.Megaparsec.Char (letterChar)
import Text.Megaparsec.Error (errorBundlePretty)

-- | Script subtags
--
-- Script subtags are used to indicate the script or writing system
-- variations that distinguish the written forms of a language or its
-- dialects.
--
newtype Script = Script { scriptToText :: Text }
  deriving stock (Show, Eq, Ord)

instance Arbitrary Script where
  arbitrary = Script . pack <$> alphaString 4

-- | Parse a 'Script' subtag from 'Text'
scriptFromText :: Text -> Either Text Script
scriptFromText =
  first (pack . errorBundlePretty) . parse scriptP "scriptFromText"

-- | BCP-47 script parser
--
-- @@
--  script        = 4ALPHA              ; ISO 15924 code
-- @@
--
scriptP :: Parsec Void Text Script
scriptP = complete $ Script . pack <$> count 4 letterChar
