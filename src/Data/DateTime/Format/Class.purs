module Data.DateTime.Format.Class
where

import Data.DateTime

class FormatTime a where
  getHour :: a -> Hour
  getMinute :: a -> Minute
  getSecond :: a -> Second
  getMillisecond :: a -> Millisecond

class FormatDate a where
  getYear :: a -> Year
  getMonth :: a -> Month
  getDay :: a -> Day
  getWeekday :: a -> Weekday

class (FormatTime a, FormatDate a) <= FormatDateTime a

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
