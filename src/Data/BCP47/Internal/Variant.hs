{-# LANGUAGE OverloadedStrings #-}

module Data.BCP47.Internal.Variant
  ( Variant(Variant)
  , variantFromText
  , variantToText
  , variantP
  )
  where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.Text (Text, pack)
import Data.Void (Void)
import Test.QuickCheck.Arbitrary
import Text.Megaparsec (Parsec, count, count', parse, try)
import Text.Megaparsec.Char (alphaNumChar, digitChar)
import Text.Megaparsec.Error (parseErrorPretty)

-- | BCP-47 variant parser
--
-- @@
-- variant       = 5*8alphanum         ; registered variants
--               / (DIGIT 3alphanum)
-- @@
--
variantP :: Parsec Void Text Variant
variantP = Variant . pack <$> (try (count' 5 8 alphaNumChar) <|> digitPrefixed)
 where
  digitPrefixed = do
    x <- digitChar
    xs <- count 3 alphaNumChar
    pure $ x : xs

newtype Variant = Variant { variantToText :: Text }
  deriving (Show, Eq, Ord)

instance Arbitrary Variant where
  arbitrary = Variant . pack <$> arbitrary

variantFromText :: Text -> Either Text Variant
variantFromText =
  first (pack . parseErrorPretty) . parse variantP "variantFromText"

