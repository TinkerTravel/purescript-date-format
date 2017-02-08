module Data.DateTime.Format.Class
where

import Prelude
import Data.DateTime
import Data.DateTime.Locale (Locale (..), LocaleName (..), LocalValue (..))
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Time.Duration (Minutes (..))
import Data.Maybe (Maybe (..))

class FormatTime a where
  getHour :: a -> Hour
  getMinute :: a -> Minute
  getSecond :: a -> Second
  getMillisecond :: a -> Millisecond
  getTimeZone :: a -> Locale

class FormatDate a where
  getYear :: a -> Year
  getMonth :: a -> Month
  getDay :: a -> Day
  getWeekday :: a -> Weekday

class (FormatTime a, FormatDate a) <= FormatDateTime a where
  getTimestamp :: a -> Instant

instance formatDateDate :: FormatDate Date where
  getYear = year
  getMonth = month
  getDay = day
  getWeekday = weekday

instance formatTimeTime :: FormatTime Time where
  getHour = hour
  getMinute = minute
  getSecond = second
  getMillisecond = millisecond
  getTimeZone _ =
    Locale
      (Just $ LocaleName "UTC")
      (Minutes 0.0)

instance formatDateDateTime :: FormatDate DateTime where
  getYear = year <<< date
  getMonth = month <<< date
  getDay = day <<< date
  getWeekday = weekday <<< date

instance formatTimeDateTime :: FormatTime DateTime where
  getHour = hour <<< time
  getMinute = minute <<< time
  getSecond = second <<< time
  getMillisecond = millisecond <<< time
  getTimeZone = getTimeZone <<< time

instance formatDateTimeDateTime :: FormatDateTime DateTime where
  getTimestamp = Instant.fromDateTime

instance localValueFormatTime :: FormatTime a => FormatTime (LocalValue a) where
  getHour (LocalValue _ a) = getHour a
  getMinute (LocalValue _ a) = getMinute a
  getSecond (LocalValue _ a) = getSecond a
  getMillisecond (LocalValue _ a) = getMillisecond a
  getTimeZone (LocalValue locale a) = locale
