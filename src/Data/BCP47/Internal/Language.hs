{-# LANGUAGE OverloadedStrings #-}

module Data.BCP47.Internal.Language
  ( ISO639_1
  , languageP
  )
where

import Data.LanguageCodes (ISO639_1, fromChars)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import Text.Megaparsec.Char (lowerChar)

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
  mCode <- fromChars <$> lowerChar <*> lowerChar
  maybe (fail "unknown ISO-639-1 code") pure mCode
