{-# LANGUAGE OverloadedStrings #-}

module Data.BCP47
  ( BCP47(..)
  , mkLanguage
  , mkLocalized
  , fromText
  -- * Predicates
  , isLessConstrainedThan
  -- * Components
  , ISO639_1
  , languageToText
  , languageFromText
  , LanguageExtension
  , languageExtensionToText
  , languageExtensionFromText
  , Script
  , scriptToText
  , scriptFromText
  , CountryCode
  , regionToText
  , regionFromText
  , Variant
  , variantToText
  , variantFromText
  , Extension
  , extensionToText
  , extensionFromText
  , PrivateUse
  , privateUseToText
  , privateUseFromText
  -- * For testing
  , en
  , es
  , enGB
  , enUS
  , enTJP
  )
where

import Control.Applicative ((<|>))
import Control.Monad (MonadPlus)
import Data.BCP47.Internal.Extension
import Data.BCP47.Internal.Language
import Data.BCP47.Internal.LanguageExtension
import Data.BCP47.Internal.PrivateUse
import Data.BCP47.Internal.Region
import Data.BCP47.Internal.Script
import Data.BCP47.Internal.Variant
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.ISO3166_CountryCodes (CountryCode(GB, US))
import Data.LanguageCodes (ISO639_1(EN, ES))
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
  (Parsec, eof, hidden, many, notFollowedBy, optional, parse, try)
import Text.Megaparsec.Char (char, letterChar)
import Text.Megaparsec.Error (parseErrorPretty)

-- | BCP-47
--
-- https://tools.ietf.org/html/bcp47
--
data BCP47
  = BCP47
  { language :: ISO639_1
  , extendedLanguageSubtags :: Set LanguageExtension
  , script :: Maybe Script
  , region :: Maybe CountryCode
  , variants :: Set Variant
  , extensions :: Set Extension
  , privateUse :: Set PrivateUse
  }
  deriving (Eq)

instance Show BCP47 where
  show b = T.unpack $ T.concat
    [ languageToText (language b)
    , fromSet languageExtensionToText extendedLanguageSubtags
    , may scriptToText script
    , may regionToText region
    , fromSet variantToText variants
    , fromSet extensionToText extensions
    , if Set.null (privateUse b) then "" else "-x"
    , fromSet privateUseToText privateUse
    ]
   where
    may f g = fromList f . toList $ g b
    fromSet f g = fromList f . Set.toList $ g b
    fromList f = T.concat . fmap (("-" <>) . f)

mkLanguage :: ISO639_1 -> BCP47
mkLanguage lang = BCP47 lang mempty Nothing Nothing mempty mempty mempty

mkLocalized :: ISO639_1 -> CountryCode -> BCP47
mkLocalized lang locale =
  BCP47 lang mempty Nothing (Just locale) mempty mempty mempty

-- | Parse a language tag from text
--
-- >>> fromText $ pack "en"
-- Right en
--
-- >>> fromText $ pack "de-CH"
-- Right de-CH
--
-- >>> fromText $ pack "ru-USSSR"
-- Left "fromText:1:8:\nunexpected 'R'\nexpecting '-'\n"
--
-- >>> fromText $ pack "en-a-ccc-v-qqq-a-bbb"
-- Right en-a-bbb-a-ccc-v-qqq
--
-- >>> fromText $ pack "de-Latn-DE"
-- Right de-Latn-DE
--
-- >>> fromText $ pack "de-Latf-DE"
-- Right de-Latf-DE
--
-- >>> fromText $ pack "de-CH-1996"
-- Right de-CH-1996
--
-- >>> fromText $ pack "de-Deva"
-- Right de-Deva
--
-- >>> fromText $ pack "zh-Hant-CN-x-private1-private2"
-- Right zh-Hant-CN-x-private1-private2
--
-- >>> fromText $ pack "zh-Hant-CN-x-private1"
-- Right zh-Hant-CN-x-private1
--
-- >>> fromText $ pack "zh-Hant-CN"
-- Right zh-Hant-CN
--
-- >>> fromText $ pack "zh-Hant"
-- Right zh-Hant
--
-- >>> fromText $ pack "zh"
-- Right zh
--
fromText :: Text -> Either Text BCP47
fromText = first (pack . parseErrorPretty) . parse parser "fromText"

parser :: Parsec Void Text BCP47
parser =
  BCP47
    <$> languageP
    <*> manyAsSet
          (try (char '-' *> languageExtensionP <* notFollowedBy letterChar))
    <*> (try (optional $ char '-' *> scriptP) <|> pure Nothing)
    <*> (try (optional $ char '-' *> regionP) <|> pure Nothing)
    <*> manyAsSet (try (char '-' *> variantP))
    <*> manyAsSet (try (char '-' *> extensionP))
    <*> (try (char '-' *> privateUseP) <|> mempty)
    <* hidden eof

manyAsSet :: (Ord a, MonadPlus m) => m a -> m (Set a)
manyAsSet f = Set.fromList <$> many f

es :: BCP47
es = mkLanguage ES

en :: BCP47
en = mkLanguage EN

enGB :: BCP47
enGB = mkLocalized EN GB

enUS :: BCP47
enUS = mkLocalized EN US

enTJP :: BCP47
enTJP = en { extensions = Set.singleton (Extension (pack "t-jp")) }

-- | Check if a language tag is less constrained than another
--
-- Note: A tag may be less constrained than another tag, but this does not imply
-- that the other is more constrained. In other words `isMoreConstrainedThan` is
-- not validly defined as:
--
-- @
-- isMoreConstrainedThan x y = not $ isLessConstrainedThan x y
-- @
--
-- >>> en `isLessConstrainedThan` es
-- False
--
-- >>> es `isLessConstrainedThan` en
-- False
--
-- >>> en `isLessConstrainedThan` enGB
-- True
--
-- >>> enGB `isLessConstrainedThan` en
-- False
--
-- >>> enGB `isLessConstrainedThan` enUS
-- False
--
-- >>> en `isLessConstrainedThan` enTJP
-- True
--
-- >>> enTJP `isLessConstrainedThan` en
-- False
--
isLessConstrainedThan :: BCP47 -> BCP47 -> Bool
isLessConstrainedThan x y =
  sameLang
    && isSubsetBy privateUse
    && isSubsetBy extensions
    && isSubsetBy variants
    && isUnConstrainedEqual region
    && isUnConstrainedEqual script
    && isSubsetBy extendedLanguageSubtags
 where
  sameLang = language x == language y
  isSubsetBy f = f x `Set.isSubsetOf` f y
  isUnConstrainedEqual f = isNothing (f x) || f x == f y
