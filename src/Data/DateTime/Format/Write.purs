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
