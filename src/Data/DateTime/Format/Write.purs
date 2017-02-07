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


writeFormatItem :: forall d i.
                   (i -> d -> String)
                -> FormatItem i
                -> d
                -> String
writeFormatItem _ (Literal str) _ = str
writeFormatItem fmt (FormatItem i) d = fmt i d

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
writeDateField (MonthNameField Abbreviated) =
      getMonth
  >>> shortMonthName
writeDateField (MonthNameField Full) =
      getMonth
  >>> fullMonthName
writeDateField (DayField padding) =
      getDay
  >>> fromEnum
  >>> show
  >>> applyPadding 2 padding
writeDateField (WeekdayField Abbreviated) =
      getWeekday
  >>> shortWeekdayName
writeDateField (WeekdayField Full) =
      getWeekday
  >>> fullWeekdayName


writeTimeField :: forall t. FormatTime t
               => TimeField
               -> t
               -> String
writeTimeField (HoursField Hours24 padding) =
      getHour
  >>> fromEnum
  >>> show
  >>> applyPadding 2 padding
writeTimeField (HoursField Hours12 padding) =
      getHour
  >>> fromEnum
  >>> wrap12
  >>> show
  >>> applyPadding 2 padding
writeTimeField (MinutesField padding) =
      getMinute
  >>> fromEnum
  >>> show
  >>> applyPadding 2 padding
writeTimeField (SecondsField padding) =
      getSecond
  >>> fromEnum
  >>> show
  >>> applyPadding 2 padding
writeTimeField (MillisecondsField padding) =
      getMillisecond
  >>> fromEnum
  >>> show
  >>> applyPadding 3 padding
writeTimeField AMPMField =
      getHour
  >>> ampmMarker

ampmMarker :: Hour -> String
ampmMarker h
  | fromEnum h >= 12 = "PM"
  | otherwise = "AM"

  {-
  = HoursField HoursStyle Padding
  | MinutesField Padding
  | SecondsField Padding
  | MillisecondsField
  | AMPMField
  -}


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

shortMonthName :: Month -> String
shortMonthName = show >>> String.take 3

fullMonthName :: Month -> String
fullMonthName = show

shortWeekdayName :: Weekday -> String
shortWeekdayName = show >>> String.take 3

fullWeekdayName :: Weekday -> String
fullWeekdayName = show
