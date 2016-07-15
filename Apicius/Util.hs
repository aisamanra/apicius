{-# LANGUAGE DefaultSignatures #-}

module Apicius.Util where

import           Data.Monoid ((<>))
import           Data.Text.Buildable (Buildable(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (toLazyText)

class TShow s where
  text :: s -> Text

  default text :: Buildable s => s -> Text
  text = toStrict . toLazyText . build

instance TShow Int where
instance TShow Text where

instance TShow a => TShow [a] where
  text xs = "[" <> T.intercalate ", " (map text xs) <> "]"

instance TShow a => TShow (Maybe a) where
  text Nothing = "Nothing"
  text (Just a) = "Just " <> text a
