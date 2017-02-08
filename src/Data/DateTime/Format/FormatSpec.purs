module Data.DateTime.Format.FormatSpec
where

import Prelude
import Data.Generic
import Data.DateTime.Format.Field

data FormatItem a
  = Literal String
  | FormatItem a

derive instance genericFormatItem :: Generic a => Generic (FormatItem a)

instance showFormatItem :: Show a => Show (FormatItem a) where
  show (Literal s) = "Literal " <> show s
  show (FormatItem i) = "FormatItem " <> show i

derive instance eqFormatItem :: Eq a => Eq (FormatItem a)

derive instance functorFormatItem :: Functor FormatItem

type DateFormatSpec = Array (FormatItem DateField)

type TimeFormatSpec = Array (FormatItem TimeField)

type DateTimeFormatSpec = Array (FormatItem DateTimeField)

defTimeFormat24h :: TimeFormatSpec
defTimeFormat24h =
  [ FormatItem $ HourField Hours24 (PadWith '0')
  , Literal ":"
  , FormatItem $ MinuteField (PadWith '0')
  , Literal ":"
  , FormatItem $ SecondField (PadWith '0')
  ]

defTimeFormat12h :: TimeFormatSpec
defTimeFormat12h =
  [ FormatItem $ HourField Hours12 (PadWith '0')
  , Literal ":"
  , FormatItem $ MinuteField (PadWith '0')
  , Literal ":"
  , FormatItem $ SecondField (PadWith '0')
  , Literal " "
  , FormatItem $ AMPMField DefaultCasing
  ]
