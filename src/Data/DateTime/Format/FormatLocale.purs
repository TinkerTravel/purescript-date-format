module Data.DateTime.Format.FormatLocale
where

import Prelude
import Data.Date (Weekday (..), Month (..))
import Data.String as String

type WithDateFormatLocale r =
  ( weekdayName :: Weekday -> String
  , shortWeekdayName :: Weekday -> String
  , monthName :: Month -> String
  , shortMonthName :: Month -> String
  | r
  )

type WithTimeFormatLocale r =
  ( amName :: String
  , pmName :: String
  | r
  )

type DateFormatLocale =
  Record (WithDateFormatLocale ())

type TimeFormatLocale =
  Record (WithTimeFormatLocale ())

type DateTimeFormatLocale =
  Record (WithDateFormatLocale (WithTimeFormatLocale ()))

defDateTimeFormatLocale :: DateTimeFormatLocale
defDateTimeFormatLocale =
  { weekdayName: show
  , shortWeekdayName: String.take 3 <<< show
  , monthName: show
  , shortMonthName: String.take 3 <<< show
  , amName: "AM"
  , pmName: "PM"
  }

type WeekdayNames =
  { monday :: String
  , tuesday :: String
  , wednesday :: String
  , thursday :: String
  , friday :: String
  , saturday :: String
  , sunday :: String
  }

mkWeekdayLookup :: WeekdayNames -> Weekday -> String
mkWeekdayLookup names Monday = names.monday
mkWeekdayLookup names Tuesday = names.tuesday
mkWeekdayLookup names Wednesday = names.wednesday
mkWeekdayLookup names Thursday = names.thursday
mkWeekdayLookup names Friday = names.friday
mkWeekdayLookup names Saturday = names.saturday
mkWeekdayLookup names Sunday = names.sunday

type MonthNames =
  { january :: String
  , february :: String
  , march :: String
  , april :: String
  , may :: String
  , june :: String
  , july :: String
  , august :: String
  , september :: String
  , october :: String
  , november :: String
  , december :: String
  }

mkMonthLookup :: MonthNames -> Month -> String
mkMonthLookup names January = names.january
mkMonthLookup names February = names.february
mkMonthLookup names March = names.march
mkMonthLookup names April = names.april
mkMonthLookup names May = names.may
mkMonthLookup names June = names.june
mkMonthLookup names July = names.july
mkMonthLookup names August = names.august
mkMonthLookup names September = names.september
mkMonthLookup names October = names.october
mkMonthLookup names November = names.november
mkMonthLookup names December = names.december

mkDateTimeFormatLocale :: WeekdayNames
                       -> WeekdayNames
                       -> MonthNames
                       -> MonthNames
                       -> String
                       -> String
                       -> DateTimeFormatLocale
mkDateTimeFormatLocale weekdays shortWeekdays
                       months shortMonths
                       am pm =
  { weekdayName: mkWeekdayLookup weekdays
  , shortWeekdayName: mkWeekdayLookup shortWeekdays
  , monthName: mkMonthLookup months
  , shortMonthName: mkMonthLookup shortMonths
  , amName: am
  , pmName: pm
  }
