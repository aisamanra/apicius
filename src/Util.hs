{-# LANGUAGE DefaultSignatures #-}

module Util where

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
