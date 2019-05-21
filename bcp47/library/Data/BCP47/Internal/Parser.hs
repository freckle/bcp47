module Data.BCP47.Internal.Parser
  ( complete
  ) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, lookAhead)
import Text.Megaparsec.Char (char)

-- | Ensure a specifier extends to the next '-' or end of input
--
-- Used for specifiers that can match some prefix of another specifier.
-- For example, a @'Script'@ or @'Region'@ can accidentally be parsed
-- from the prefix of a @'Variant'@
--
-- The alternative would be to use @'notFollowedBy'@ with knowledge of
-- the legal characters in the next valid specifier.
--
complete :: Parsec Void Text a -> Parsec Void Text a
complete parser = parser <* lookAhead (void (char '-') <|> eof)
