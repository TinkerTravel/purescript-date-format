module Data.DateTime.Format.FormatSpec
where

import Data.DateTime.Format.Field

data FormatItem a
  = Literal String
  | FormatItem a

type DateFormatSpec = Array (FormatItem DateField)

type TimeFormatSpec = Array (FormatItem TimeField)

type DateTimeFormatSpec = Array (FormatItem DateTimeField)
