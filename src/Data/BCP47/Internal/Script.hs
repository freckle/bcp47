{-# LANGUAGE OverloadedStrings #-}

module Data.BCP47.Internal.Script
  ( Script(Script)
  , scriptFromText
  , scriptToText
  , scriptP
  )
where

import Data.Bifunctor (first)
import Data.Text (Text, pack)
import Data.Void (Void)
import Test.QuickCheck.Arbitrary
import Text.Megaparsec (Parsec, count, parse)
import Text.Megaparsec.Char (letterChar)
import Text.Megaparsec.Error (parseErrorPretty)

newtype Script = Script { scriptToText :: Text }
  deriving (Show, Eq, Ord)

instance Arbitrary Script where
  arbitrary = Script . pack <$> arbitrary

scriptFromText :: Text -> Either Text Script
scriptFromText =
  first (pack . parseErrorPretty) . parse scriptP "scriptFromText"

-- | BCP-47 script parser
--
-- @@
--  script        = 4ALPHA              ; ISO 15924 code
-- @@
--
scriptP :: Parsec Void Text Script
scriptP = Script . pack <$> count 4 letterChar
