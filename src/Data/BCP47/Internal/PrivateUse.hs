{-# LANGUAGE OverloadedStrings #-}

module Data.BCP47.Internal.PrivateUse
  ( PrivateUse(PrivateUse)
  , privateUseFromText
  , privateUseToText
  , privateUseP
  )
where

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (Parsec, count', parse, some)
import Text.Megaparsec.Char (alphaNumChar, char)
import Text.Megaparsec.Error (parseErrorPretty)

newtype PrivateUse = PrivateUse { privateUseToText :: Text }
  deriving (Show, Eq, Ord)

privateUseFromText :: Text -> Either Text (Set PrivateUse)
privateUseFromText =
  first (pack . parseErrorPretty) . parse privateUseP "scriptFromText"

-- | BCP-47 private use parser
--
-- @@
-- privateuse    = "x" 1*("-" (1*8alphanum))
-- @@
--
privateUseP :: Parsec Void Text (Set PrivateUse)
privateUseP = do
  void $ char 'x'
  rest <- some (char '-' *> count' 1 8 alphaNumChar)
  pure $ Set.fromList $ PrivateUse . pack <$> rest
