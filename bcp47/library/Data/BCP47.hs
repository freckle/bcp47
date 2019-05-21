{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.BCP47
  ( BCP47
  , language
  , extendedLanguageSubtags
  , script
  , region
  , variants
  , extensions
  , privateUse
  , toSpecifiers
  , inits
  -- * Construction
  , mkLanguage
  , mkLocalized
  , fromText
  -- * Serialization
  , toText
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
  , enGBTJP
  )
where

import Control.Applicative ((<|>))
import Control.Monad (MonadPlus)
import Data.BCP47.Internal.Arbitrary
  (Arbitrary, arbitrary, choose, elements, listOf, vectorOf)
import Data.BCP47.Internal.Extension
import Data.BCP47.Internal.Language
import Data.BCP47.Internal.LanguageExtension
import Data.BCP47.Internal.PrivateUse
import Data.BCP47.Internal.Region
import Data.BCP47.Internal.Script
import Data.BCP47.Internal.Specifiers
import Data.BCP47.Internal.Variant
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.ISO3166_CountryCodes (CountryCode(GB, US))
import Data.LanguageCodes (ISO639_1(EN, ES))
import qualified Data.List as List
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, hidden, many, optional, parse, try)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Error (errorBundlePretty)

-- | BCP-47
--
-- https://tools.ietf.org/html/bcp47
--
data BCP47
  = BCP47
  { language :: ISO639_1
  , specifiers :: Set Specifiers
  }
  deriving (Eq, Ord)

instance Arbitrary BCP47 where
  arbitrary = BCP47
    <$> elements [EN, ES]
    <*> specs
   where
    oneOrNone f =
      choose (0,1) >>= (`vectorOf` (f <$> arbitrary))
    manyOf f = listOf (f <$> arbitrary)
    specs = Set.fromList . mconcat <$> sequenceA
      [ manyOf SpecifyLanguageExtension
      , oneOrNone SpecifyScript
      , choose (0,1) >>= (`vectorOf` (elements $ SpecifyRegion <$> [US, GB]))
      , manyOf SpecifyVariant
      , manyOf SpecifyExtension
      , oneOrNone SpecifyPrivateUse
      ]

instance Show BCP47 where
  show = T.unpack . toText

toText :: BCP47 -> Text
toText b = T.intercalate "-" $ mconcat
  [ [languageToText $ language b]
  , mapMaybe fromSpecifiers . Set.toList $ specifiers b
  , if Set.null (privateUse b) then [] else ["x"]
  , map privateUseToText . Set.toList $ privateUse b
  ]
 where
  fromSpecifiers = \case
    SpecifyLanguageExtension x -> Just $ languageExtensionToText x
    SpecifyScript x -> Just $ scriptToText x
    SpecifyRegion x -> Just $ regionToText x
    SpecifyVariant x -> Just $ variantToText x
    SpecifyExtension x -> Just $ extensionToText x
    SpecifyPrivateUse _ -> Nothing

extendedLanguageSubtags :: BCP47 -> Set LanguageExtension
extendedLanguageSubtags = asSet $ \case
  SpecifyLanguageExtension x -> Just x
  _otherwise -> Nothing

script :: BCP47 -> Maybe Script
script = headMay . mapMaybe f . Set.toList . specifiers
 where
  f = \case
    SpecifyScript x -> Just x
    _otherwise -> Nothing

region :: BCP47 -> Maybe CountryCode
region = headMay . mapMaybe f . Set.toList . specifiers
 where
  f = \case
    SpecifyRegion x -> Just x
    _otherwise -> Nothing

variants :: BCP47 -> Set Variant
variants = asSet $ \case
  SpecifyVariant x -> Just x
  _otherwise -> Nothing

extensions :: BCP47 -> Set Extension
extensions = asSet $ \case
  SpecifyExtension x -> Just x
  _otherwise -> Nothing

privateUse :: BCP47 -> Set PrivateUse
privateUse = asSet $ \case
  SpecifyPrivateUse x -> Just x
  _otherwise -> Nothing

asSet :: Ord a => (Specifiers -> Maybe a) -> BCP47 -> Set a
asSet f = Set.fromList . mapMaybe f . Set.toList . specifiers

headMay :: [x] -> Maybe x
headMay [] = Nothing
headMay (x : _) = Just x

toSpecifiers :: BCP47 -> [Specifiers]
toSpecifiers tag = toList $ specifiers tag

-- | Produce a list of `(<= priority)` language tags
--
-- >>> inits enGBTJP
-- [en,en-GB,en-GB-t-jp]
--
inits :: BCP47 -> [BCP47]
inits tag =
  map (BCP47 (language tag) . Set.fromList) . List.inits $ toSpecifiers tag

mkLanguage :: ISO639_1 -> BCP47
mkLanguage lang = BCP47 lang mempty

mkLocalized :: ISO639_1 -> CountryCode -> BCP47
mkLocalized lang locale = BCP47 lang . Set.singleton $ SpecifyRegion locale

-- | Parse a language tag from text
--
-- >>> fromText $ pack "en"
-- Right en
--
-- >>> fromText $ pack "de-CH"
-- Right de-CH
--
-- >>> fromText $ pack "ru-USR"
-- Left "fromText:1:3:\n  |\n1 | ru-USR\n  |   ^\nunexpected '-'\n"
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
fromText = first (pack . errorBundlePretty) . parse parser "fromText"

parser :: Parsec Void Text BCP47
parser = BCP47 <$> languageP <*> specifiersP <* hidden eof
 where
  specifiersP = mconcat <$> sequenceA
    [ manyAsSet SpecifyLanguageExtension (try (char '-' *> languageExtensionP))
    , maybe mempty (Set.singleton . SpecifyScript)
      <$> (try (optional $ char '-' *> scriptP) <|> pure Nothing)
    , maybe mempty (Set.singleton . SpecifyRegion)
      <$> (try (optional $ char '-' *> regionP) <|> pure Nothing)
    , manyAsSet SpecifyVariant (try (char '-' *> variantP))
    , manyAsSet SpecifyExtension (try (char '-' *> extensionP))
    , Set.map SpecifyPrivateUse <$> (try (char '-' *> privateUseP) <|> mempty)
    ]

manyAsSet :: (Ord b, MonadPlus m) => (a -> b) -> m a -> m (Set b)
manyAsSet f p = Set.fromList . map f <$> many p

es :: BCP47
es = mkLanguage ES

en :: BCP47
en = mkLanguage EN

enGB :: BCP47
enGB = mkLocalized EN GB

enUS :: BCP47
enUS = mkLocalized EN US

enTJP :: BCP47
enTJP = en
  { specifiers = Set.insert (SpecifyExtension (Extension (pack "t-jp")))
    $ specifiers en
  }

enGBTJP :: BCP47
enGBTJP = enGB
  { specifiers = Set.insert (SpecifyExtension (Extension (pack "t-jp")))
    $ specifiers enGB
  }
