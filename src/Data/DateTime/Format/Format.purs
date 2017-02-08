module Data.DateTime.Format.Format
where

import Prelude
import Data.DateTime.Format.Field
import Data.DateTime.Format.FormatSpec
import Data.DateTime.Format.Parse
import Data.DateTime.Format.Class
import Data.DateTime.Format.Write
import Data.Date (Weekday (..))
import Data.Either (Either (..))


formatDate :: forall d. FormatDate d
           => String
           -> d
           -> Either String String
formatDate fmt d = do
  spec <- parseDateFormat fmt
  pure $ writeDateFormat spec d

formatTime :: forall d. FormatTime d
           => String
           -> d
           -> Either String String
formatTime fmt d = do
  spec <- parseTimeFormat fmt
  pure $ writeTimeFormat spec d

formatDateTime :: forall d. FormatDateTime d
           => String
           -> d
           -> Either String String
formatDateTime fmt d = do
  spec <- parseDateTimeFormat fmt
  pure $ writeDateTimeFormat spec d

