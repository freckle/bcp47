{-# LANGUAGE OverloadedStrings #-}

module Data.BCP47.Internal.Extension
  ( Extension(Extension)
  , extensionFromText
  , extensionToText
  , extensionP
  )
where

import Control.Monad (void, when)
import Data.BCP47.Internal.Arbitrary
  (Arbitrary, alphaChar, alphaNumString, arbitrary, choose, suchThat)
import Data.BCP47.Internal.Parser (complete)
import Data.Bifunctor (first)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (Parsec, count', parse)
import Text.Megaparsec.Char (alphaNumChar, char)
import Text.Megaparsec.Error (errorBundlePretty)

newtype Extension = Extension { extensionToText :: Text }
  deriving (Show, Eq, Ord)

instance Arbitrary Extension where
  arbitrary = do
    prefix <- alphaChar `suchThat` (`notElem` ['x', 'X'])
    len <- choose (2,8)
    chars <- alphaNumString len
    pure . Extension . pack $ prefix : '-' : chars

extensionFromText :: Text -> Either Text Extension
extensionFromText =
  first (pack . errorBundlePretty) . parse extensionP "extensionFromText"

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
extensionP = complete $ do
  ext <- alphaNumChar
  when (ext `elem` ['x', 'X']) $ fail "private use suffix found"
  void $ char '-'
  rest <- count' 2 8 alphaNumChar
  pure . Extension . pack $ ext : '-' : rest
