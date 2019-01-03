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
  -- * For testing
  , en
  , es
  , enGB
  , enUS
  , enTJP
  )
where

import Control.Applicative ((<|>))
import Control.Monad (MonadPlus, void, when)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.ISO3166_CountryCodes (CountryCode(GB, US))
import Data.LanguageCodes (ISO639_1(EN, ES), fromChars)
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec
  , count
  , count'
  , eof
  , hidden
  , many
  , notFollowedBy
  , optional
  , parse
  , some
  , try
  )
import Text.Megaparsec.Char
  (alphaNumChar, char, letterChar, lowerChar, upperChar)
import Text.Megaparsec.Error (parseErrorPretty)
import Text.Read (readEither)

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

instance Show BCP47 where
  show b = T.unpack $ T.concat
    [ T.toLower (tshow (language b))
    , fromSet languageExtensionToText extendedLanguageSubtags
    , may scriptToText script
    , may tshow region
    , fromSet variantToText variants
    , fromSet extensionToText extensions
    , fromSet (const "x") privateUse
    , fromSet privateUseToText privateUse
    ]
   where
    tshow :: Show a => a -> Text
    tshow = pack . show
    may f g = fromList f . toList $ g b
    fromSet f g = fromList f . Set.toList $ g b
    fromList f = T.concat . fmap (("-" <>) . f)

newtype LanguageExtension = LanguageExtension { languageExtensionToText :: Text }
  deriving (Show, Eq, Ord)

newtype Script = Script { scriptToText :: Text }
  deriving (Show, Eq, Ord)

newtype Variant = Variant { variantToText :: Text }
  deriving (Show, Eq, Ord)

newtype Extension = Extension { extensionToText :: Text }
  deriving (Show, Eq, Ord)

newtype PrivateUse = PrivateUse { privateUseToText :: Text }
  deriving (Show, Eq, Ord)

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
    <*> manyAsSet (try (char '-' *> languageExtP <* notFollowedBy letterChar))
    <*> (try (optional $ char '-' *> scriptP) <|> pure Nothing)
    <*> (try (optional $ char '-' *> regionP) <|> pure Nothing)
    <*> manyAsSet (try (char '-' *> variantP))
    <*> manyAsSet (try (char '-' *> extensionP))
    <*> manyAsSet (try (char '-' *> privateUseP))
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
-- NOTE: parsers lists as 5*8, but 4*8 seems to be more accurate
--
variantP :: Parsec Void Text Variant
variantP = Variant . pack <$> count' 4 8 alphaNumChar

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
privateUseP = PrivateUse <$> do
  void $ char 'x'
  rest <- some (char '-' *> count' 1 8 alphaNumChar)
  pure $ T.intercalate "-" (pack <$> rest)
