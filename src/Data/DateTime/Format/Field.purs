module Data.DateTime.Format.Field
where

data HoursStyle
  = Hours12
  | Hours24

data Padding
  = NoPadding
  | PadWith Char

data Abbreviated
  = Abbreviated
  | Full

data TimeField
  = HoursField HoursStyle Padding
  | MinutesField Padding
  | SecondsField Padding
  | MillisecondsField
  | AMPMField

data DateField
  = YearField Abbreviated Padding
  | MonthNumberField Padding
  | MonthNameField Abbreviated
  | DayOfMonthField Padding
  | WeekdayField Abbreviated

data DateTimeField
  = TimeField TimeField
  | DateField DateField
