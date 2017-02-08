module Data.DateTime.Format.Write
where

import Prelude
import Data.DateTime
import Data.DateTime.Format.Class
import Data.DateTime.Format.FormatSpec
import Data.DateTime.Format.Field
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.String as String
import Data.Enum (fromEnum)

writeDateFormat :: forall d. FormatDate d
                => DateFormatSpec
                -> d
                -> String
writeDateFormat fmt d =
  foldMap (\item -> writeDateFormatItem item d) fmt


writeDateFormatItem :: forall d. FormatDate d
                    => (FormatItem DateField)
                    -> d
                    -> String
writeDateFormatItem = writeFormatItem writeDateField


writeTimeFormat :: forall d. FormatTime d
                => TimeFormatSpec
                -> d
                -> String
writeTimeFormat fmt d =
  foldMap (\item -> writeTimeFormatItem item d) fmt


writeTimeFormatItem :: forall d. FormatTime d
                    => (FormatItem TimeField)
                    -> d
                    -> String
writeTimeFormatItem = writeFormatItem writeTimeField


writeDateTimeFormat :: forall d. FormatDateTime d
                => DateTimeFormatSpec
                -> d
                -> String
writeDateTimeFormat fmt d =
  foldMap (\item -> writeDateTimeFormatItem item d) fmt


writeDateTimeFormatItem :: forall d. FormatDateTime d
                    => (FormatItem DateTimeField)
                    -> d
                    -> String
writeDateTimeFormatItem = writeFormatItem writeDateTimeField


writeFormatItem :: forall d i.
                   (i -> d -> String)
                -> FormatItem i
                -> d
                -> String
writeFormatItem _ (Literal str) _ = str
writeFormatItem fmt (FormatItem i) d = fmt i d

writeDateTimeField :: forall d. FormatDateTime d
               => DateTimeField
               -> d
               -> String
writeDateTimeField (DateField f) = writeDateField f
writeDateTimeField (TimeField f) = writeTimeField f

writeDateField :: forall d. FormatDate d
               => DateField
               -> d
               -> String
writeDateField (YearField Full padding) =
      getYear
  >>> fromEnum
  >>> show
  >>> applyPadding 4 padding
writeDateField (YearField Abbreviated padding) =
      getYear
  >>> fromEnum
  >>> show
  >>> takeEnd 2
  >>> applyPadding 2 padding
writeDateField (MonthNumberField padding) =
      getMonth
  >>> fromEnum
  >>> show
  >>> applyPadding 2 padding
writeDateField (MonthNameField Abbreviated casing) =
      getMonth
  >>> shortMonthName
  >>> applyCasing casing
writeDateField (MonthNameField Full casing) =
      getMonth
  >>> fullMonthName
  >>> applyCasing casing
writeDateField (DayField padding) =
      getDay
  >>> fromEnum
  >>> show
  >>> applyPadding 2 padding
writeDateField (WeekdayNameField Abbreviated casing) =
      getWeekday
  >>> shortWeekdayName
  >>> applyCasing casing
writeDateField (WeekdayNameField Full casing) =
      getWeekday
  >>> fullWeekdayName
  >>> applyCasing casing
writeDateField (WeekdayNumberField shift base) =
      getWeekday
  >>> fromEnum
  >>> (\x -> (x + 7 - fromEnum shift) `mod` 7 + base)
  >>> show


writeTimeField :: forall t. FormatTime t
               => TimeField
               -> t
               -> String
writeTimeField (HourField Hours24 padding) =
      getHour
  >>> fromEnum
  >>> show
  >>> applyPadding 2 padding
writeTimeField (HourField Hours12 padding) =
      getHour
  >>> fromEnum
  >>> wrap12
  >>> show
  >>> applyPadding 2 padding
writeTimeField (MinuteField padding) =
      getMinute
  >>> fromEnum
  >>> show
  >>> applyPadding 2 padding
writeTimeField (SecondField padding) =
      getSecond
  >>> fromEnum
  >>> show
  >>> applyPadding 2 padding
writeTimeField (MillisecondsField padding) =
      getMillisecond
  >>> fromEnum
  >>> show
  >>> applyPadding 3 padding
writeTimeField (AMPMField casing) =
      getHour
  >>> ampmMarker
  >>> applyCasing casing

ampmMarker :: Hour -> String
ampmMarker h
  | fromEnum h >= 12 = "PM"
  | otherwise = "AM"

wrap12 :: Int -> Int
wrap12 i
  | i > 12 = i - 12
  | otherwise = i

takeEnd :: Int -> String -> String
takeEnd targetLength str =
  let l = String.length str
  in if l > targetLength
      then String.drop (l - targetLength) str
      else str

applyPadding :: Int -> Padding -> String -> String
applyPadding width padding str =
  case padding of
    NoPadding -> str
    PadWith c ->
      let l = String.length str
          npad = max 0 $ width - l
          pad = String.fromCharArray $ Array.replicate npad c
      in pad <> str

applyCasing :: Casing -> String -> String
applyCasing DefaultCasing str = str
applyCasing AllCaps str = String.toUpper str
applyCasing LowerCase str = String.toLower str

shortMonthName :: Month -> String
shortMonthName = show >>> String.take 3

fullMonthName :: Month -> String
fullMonthName = show

shortWeekdayName :: Weekday -> String
shortWeekdayName = show >>> String.take 3

fullWeekdayName :: Weekday -> String
fullWeekdayName = show
