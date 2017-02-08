module Data.DateTime.Format.Parse
where

import Prelude
import Data.DateTime.Format.Field
import Data.DateTime.Format.FormatSpec
import Data.Date (Weekday (..))
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.String as P
import Data.String (fromCharArray)
import Data.Array as Array
import Data.Foldable (fold)
import Data.Traversable (sequence)
import Control.Alt (class Alt, alt, (<|>))
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Either (Either (..))

onLeft :: forall a b c. (a -> c) -> Either a b -> Either c b
onLeft f (Left x) = Left (f x)
onLeft _ (Right x) = Right x

stringOf :: P.Parser String Char -> P.Parser String String
stringOf pChar = fromCharArray <$> Array.many pChar

stringOf1 :: P.Parser String Char -> P.Parser String String
stringOf1 pChar = fromCharArray <$> Array.some pChar

parseDateFormat :: String -> Either String DateFormatSpec
parseDateFormat input =
  onLeft P.parseErrorMessage $ P.runParser input pDateFormat

parseTimeFormat :: String -> Either String TimeFormatSpec
parseTimeFormat input =
  onLeft P.parseErrorMessage $ P.runParser input pTimeFormat

parseDateTimeFormat :: String -> Either String DateTimeFormatSpec
parseDateTimeFormat input =
  onLeft P.parseErrorMessage $ P.runParser input pDateTimeFormat


pDateFormat :: P.Parser String DateFormatSpec
pDateFormat = fold <$> Array.many pDateItem <* P.eof

pTimeFormat :: P.Parser String TimeFormatSpec
pTimeFormat = fold <$> Array.many pTimeItem <* P.eof

pDateTimeFormat :: P.Parser String DateTimeFormatSpec
pDateTimeFormat = fold <$> Array.many pDateTimeItem <* P.eof


pDateItem :: P.Parser String (Array (FormatItem DateField))
pDateItem = pDateField <|> pLiteral

pTimeItem :: P.Parser String (Array (FormatItem TimeField))
pTimeItem = pTimeField <|> pLiteral

pDateTimeItem :: P.Parser String (Array (FormatItem DateTimeField))
pDateTimeItem = pDateTimeField <|> pLiteral

pLiteral :: forall a. P.Parser String (Array (FormatItem a))
pLiteral = Array.singleton <<< Literal <$> stringOf1 (P.noneOf ['%'])

-- http://www.cplusplus.com/reference/ctime/strftime/
-- http://hackage.haskell.org/package/time-1.7.0.1/docs/Data-Time-Format.html

pField :: forall a.
          (Maybe Padding -> Char -> P.Parser String (Array (FormatItem a)))
       -> P.Parser String (Array (FormatItem a))
pField inner = do
  P.char '%'
  pad <- P.optionMaybe pPadding
  c <- P.anyChar
  case c of
    '%' -> pure [Literal "%"]
    't' -> pure [Literal "\t"]
    'n' -> pure [Literal "\n"]
    c -> inner pad c

pPadding :: P.Parser String Padding
pPadding =
      (P.try $ P.char '-' *> pure NoPadding)
  <|> (P.try $ P.char '_' *> pure (PadWith ' '))
  <|> (P.try $ P.char '0' *> pure (PadWith '0'))

pDateField :: P.Parser String (Array (FormatItem DateField))
pDateField = pField mkDateField

pTimeField :: P.Parser String (Array (FormatItem TimeField))
pTimeField = pField mkTimeField

pDateTimeField :: P.Parser String (Array (FormatItem DateTimeField))
pDateTimeField = pField mkDateTimeField

mkDateField :: Maybe Padding -> Char -> P.Parser String (Array (FormatItem DateField))
mkDateField padMay c =
  case c of
    ---- Composite dates
    -- `%D`:   same as `%m/%d/%y`
    'D' -> fold <$> sequence
      [ mkDateField Nothing 'm'
      , pure [Literal "/"]
      , mkDateField Nothing 'd'
      , pure [Literal "/"]
      , mkDateField Nothing 'y'
      ]

    -- `%F`:   same as `%Y-%m-%d`
    'F' -> fold <$> sequence
      [ mkDateField Nothing 'Y'
      , pure [Literal "-"]
      , mkDateField Nothing 'm'
      , pure [Literal "-"]
      , mkDateField Nothing 'd'
      ]
    -- `%x`:   as `dateFmt` `locale` (e.g. `%m/%d/%y`)


    ---- Year
    -- `%Y`:   year, no padding. Note `%0Y` and `%_Y` pad to four chars
    'Y' -> pure [FormatItem $ YearField Full (fromMaybe NoPadding padMay)]
    -- `%y`:   year of century, 0-padded to two chars, `00` - `99`
    'y' -> pure [FormatItem $ YearField Abbreviated (fromMaybe (PadWith '0') padMay)]
    -- `%C`:   century, no padding. Note `%0C` and `%_C` pad to two chars


    ---- Month
    -- `%B`:   month name, long form (`fst` from `months` `locale`), `January` -
    --         `December`
    'B' -> pure [FormatItem $ MonthNameField Full]
    -- `%b`,
    -- `%h`:   month name, short form (`snd` from `months` `locale`), `Jan` - `Dec`
    'b' -> pure [FormatItem $ MonthNameField Abbreviated]
    'h' -> pure [FormatItem $ MonthNameField Abbreviated]
    -- `%m`:   month of year, 0-padded to two chars, `01` - `12`
    'm' -> pure [FormatItem $ MonthNumberField (PadWith '0')]


    ---- Day
    -- `%d`:   day of month, 0-padded to two chars, `01` - `31`
    'd' -> pure [FormatItem $ DayField (PadWith '0')]
    -- `%e`:   day of month, space-padded to two chars, ` 1` - `31`
    'e' -> pure [FormatItem $ DayField (PadWith ' ')]
    -- `%j`:   day of year, 0-padded to three chars, `001` - `366`

    ---- Weekday
    -- `%u`:   day of week for Week Date format, `1` - `7`
    'u' -> pure [FormatItem $ WeekdayNumberField Monday 1]
    -- `%w`:   day of week number, `0` (= Sunday) - `6` (= Saturday)
    'w' -> pure [FormatItem $ WeekdayNumberField Sunday 0]
    -- `%a`:   day of week, short form (`snd` from `wDays` `locale`), `Sun` - `Sat`
    'a' -> pure [FormatItem $ WeekdayNameField Abbreviated]
    -- `%A`:   day of week, long form (`fst` from `wDays` `locale`), `Sunday` -
    --         `Saturday`
    'A' -> pure [FormatItem $ WeekdayNameField Full]

    ---- Week numbers
    -- `%G`:   year for Week Date format, no padding. Note `%0G` and `%_G` pad to
    --         four chars
    -- `%g`:   year of century for Week Date format, 0-padded to two chars, `00` -
    --         `99`
    -- `%f`:   century for Week Date format, no padding. Note `%0f` and `%_f` pad
    --         to two chars
    -- `%V`:   week of year for Week Date format, 0-padded to two chars, `01` -
    --         `53`
    -- `%U`:   week of year where weeks start on Sunday (as `sundayStartWeek`),
    --         0-padded to two chars, `00` - `53`
    -- `%W`:   week of year where weeks start on Monday (as `mondayStartWeek`),
    --         0-padded to two chars, `00` - `53`
    _ -> P.fail $ "Invalid date format specifier " <> show c

mkTimeField :: Maybe Padding -> Char -> P.Parser String (Array (FormatItem TimeField))
mkTimeField padMay c =
  case c of
    -- `%R`:   same as `%H:%M`
    'R' -> fold <$> sequence
      [ mkTimeField Nothing 'H'
      , pure [Literal ":"]
      , mkTimeField Nothing 'M'
      ]
    -- `%T`:   same as `%H:%M:%S`
    'T' -> fold <$> sequence
      [ mkTimeField Nothing 'H'
      , pure [Literal ":"]
      , mkTimeField Nothing 'M'
      , pure [Literal ":"]
      , mkTimeField Nothing 'S'
      ]

    -- `%X`:   as `timeFmt` `locale` (e.g. `%H:%M:%S`)
    -- `%r`:   as `time12Fmt` `locale` (e.g. `%I:%M:%S %p`)
    -- `%P`:   day-half of day from (`amPm` `locale`), converted to lowercase,
    --     `am`, `pm`
    -- `%p`:   day-half of day from (`amPm` `locale`), `AM`, `PM`

    -- `%H`:   hour of day (24-hour), 0-padded to two chars, `00` - `23`
    'H' -> pure [FormatItem $ HoursField Hours24 (fromMaybe (PadWith '0') padMay)]
    -- `%k`:   hour of day (24-hour), space-padded to two chars, ` 0` - `23`
    'k' -> pure [FormatItem $ HoursField Hours24 (fromMaybe (PadWith ' ') padMay)]
    -- `%I`:   hour of day-half (12-hour), 0-padded to two chars, `01` - `12`
    'I' -> pure [FormatItem $ HoursField Hours12 (fromMaybe (PadWith '0') padMay)]
    -- `%l`:   hour of day-half (12-hour), space-padded to two chars, ` 1` - `12`
    'l' -> pure [FormatItem $ HoursField Hours12 (fromMaybe (PadWith ' ') padMay)]

    -- `%M`:   minute of hour, 0-padded to two chars, `00` - `59`
    'M' -> pure [FormatItem $ MinutesField (fromMaybe (PadWith '0') padMay)]
    -- `%S`:   second of minute (without decimal part), 0-padded to two chars, `00`
    --     - `60`
    'S' -> pure [FormatItem $ SecondsField (fromMaybe (PadWith '0') padMay)]
    -- `%q`:   picosecond of second, 0-padded to twelve chars, `000000000000` -
    --     `999999999999`.
    -- `%Q`:   decimal point and fraction of second, up to 12 second decimals,
    --     without trailing zeros. For a whole number of seconds, `%Q` produces
    --     the empty string.
    _ -> P.fail $ "Invalid time format specifier " <> show c

mkDateTimeField :: Maybe Padding
                -> Char
                -> P.Parser String (Array (FormatItem DateTimeField))
mkDateTimeField padMay c =
      (map (map TimeField) <$> mkTimeField padMay c)
  <|> (map (map DateField) <$> mkDateField padMay c)
  <|> (P.fail $ "Invalid date/time format specifier " <> show c)

{-

`%%`:   `%`
`%t`:   tab
`%n`:   newline

glibc-style modifiers can be used before the letter (here marked as
`z`):

`%-z`:   no padding
`%_z`:   pad with spaces
`%0z`:   pad with zeros
`%^z`:   convert to upper case
`%#z`:   convert to lower case (consistently, unlike glibc)

For `TimeZone` (and `ZonedTime` and `UTCTime`):

`%z`:   timezone offset in the format `-HHMM`.
`%Z`:   timezone name

For `LocalTime` (and `ZonedTime` and `UTCTime` and `UniversalTime`):

`%c`:   as `dateTimeFmt` `locale` (e.g. `%a %b %e %H:%M:%S %Z %Y`)

For `TimeOfDay` (and `LocalTime` and `ZonedTime` and `UTCTime` and
`UniversalTime`):

`%R`:   same as `%H:%M`
`%T`:   same as `%H:%M:%S`
`%X`:   as `timeFmt` `locale` (e.g. `%H:%M:%S`)
`%r`:   as `time12Fmt` `locale` (e.g. `%I:%M:%S %p`)
`%P`:   day-half of day from (`amPm` `locale`), converted to lowercase,
    `am`, `pm`
`%p`:   day-half of day from (`amPm` `locale`), `AM`, `PM`
`%H`:   hour of day (24-hour), 0-padded to two chars, `00` - `23`
`%k`:   hour of day (24-hour), space-padded to two chars, ` 0` - `23`
`%I`:   hour of day-half (12-hour), 0-padded to two chars, `01` - `12`
`%l`:   hour of day-half (12-hour), space-padded to two chars, ` 1` - `12`
`%M`:   minute of hour, 0-padded to two chars, `00` - `59`
`%S`:   second of minute (without decimal part), 0-padded to two chars, `00`
    - `60`
`%q`:   picosecond of second, 0-padded to twelve chars, `000000000000` -
    `999999999999`.
`%Q`:   decimal point and fraction of second, up to 12 second decimals,
    without trailing zeros. For a whole number of seconds, `%Q` produces
    the empty string.

For `UTCTime` and `ZonedTime`:

`%s`:   number of whole seconds since the Unix epoch. For times before the
    Unix epoch, this is a negative number. Note that in `%s.%q` and
    `%s%Q` the decimals are positive, not negative. For example, 0.9
    seconds before the Unix epoch is formatted as `-1.1` with `%s%Q`.

For `Day` (and `LocalTime` and `ZonedTime` and `UTCTime` and
`UniversalTime`):

`%D`:   same as `%m/%d/%y`
`%F`:   same as `%Y-%m-%d`
`%x`:   as `dateFmt` `locale` (e.g. `%m/%d/%y`)
`%Y`:   year, no padding. Note `%0Y` and `%_Y` pad to four chars
`%y`:   year of century, 0-padded to two chars, `00` - `99`
`%C`:   century, no padding. Note `%0C` and `%_C` pad to two chars
`%B`:   month name, long form (`fst` from `months` `locale`), `January` -
        `December`
`%b`,
`%h`:   month name, short form (`snd` from `months` `locale`), `Jan` - `Dec`
`%m`:   month of year, 0-padded to two chars, `01` - `12`
`%d`:   day of month, 0-padded to two chars, `01` - `31`
`%e`:   day of month, space-padded to two chars, ` 1` - `31`
`%j`:   day of year, 0-padded to three chars, `001` - `366`
`%G`:   year for Week Date format, no padding. Note `%0G` and `%_G` pad to
        four chars
`%g`:   year of century for Week Date format, 0-padded to two chars, `00` -
        `99`
`%f`:   century for Week Date format, no padding. Note `%0f` and `%_f` pad
        to two chars
`%V`:   week of year for Week Date format, 0-padded to two chars, `01` -
        `53`
`%u`:   day of week for Week Date format, `1` - `7`
`%a`:   day of week, short form (`snd` from `wDays` `locale`), `Sun` - `Sat`
`%A`:   day of week, long form (`fst` from `wDays` `locale`), `Sunday` -
        `Saturday`
`%U`:   week of year where weeks start on Sunday (as `sundayStartWeek`),
        0-padded to two chars, `00` - `53`
`%w`:   day of week number, `0` (= Sunday) - `6` (= Saturday)
`%W`:   week of year where weeks start on Monday (as `mondayStartWeek`),
        0-padded to two chars, `00` - `53`

-}
