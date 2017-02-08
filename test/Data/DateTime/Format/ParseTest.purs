module Data.DateTime.Format.ParseTest
where

import Prelude
import Test.Unit (TestSuite, Test, test, suite)
import Test.Unit.Assert (assert)
import Test.Unit.Assert as Assert
import Data.Array as Array
import Data.Either (Either (..), isLeft)
import Data.DateTime.Format
import Data.Tuple (Tuple (..))
import Data.Traversable (for, sequence)

parserSuite :: forall e. TestSuite e
parserSuite = do
  literalParserSuite
  timeParserSuite
  dateParserSuite

literalParserSuite :: forall e. TestSuite e
literalParserSuite = do
  let cases =
        [ Tuple "" []
        , Tuple "Hello" [Literal "Hello"]
        , Tuple "%%" [Literal "%"]
        , Tuple "%n" [Literal "\n"]
        , Tuple "%t" [Literal "\t"]
        ]
  suite "Parse literal formatters" do
    void $ for cases \(Tuple fmt expected) -> do
      test (if fmt == "" then "<empty format>" else fmt) do
        Assert.equal
          (Right expected)
          (parseDateTimeFormat fmt)

timeParserSuite :: forall e. TestSuite e
timeParserSuite = do
  let cases =
        [ Tuple "%H" [ FormatItem $ HoursField Hours24 (PadWith '0') ]
        , Tuple "%k" [ FormatItem $ HoursField Hours24 (PadWith ' ') ]
        , Tuple "%M" [ FormatItem (MinutesField (PadWith '0'))]
        , Tuple "%S" [ FormatItem (SecondsField (PadWith '0'))]
        , Tuple "%I" [ FormatItem (HoursField Hours12 (PadWith '0'))]
        , Tuple "%l" [ FormatItem (HoursField Hours12 (PadWith ' '))]
        , Tuple "%R" [ FormatItem (HoursField Hours24 (PadWith '0'))
                     , Literal ":"
                     , FormatItem (MinutesField (PadWith '0'))
                     ]
        , Tuple "%T" [ FormatItem (HoursField Hours24 (PadWith '0'))
                     , Literal ":"
                     , FormatItem (MinutesField (PadWith '0'))
                     , Literal ":"
                     , FormatItem (SecondsField (PadWith '0'))
                     ]
        , Tuple "%M-%H" [ FormatItem $ MinutesField (PadWith '0')
                        , Literal "-"
                        , FormatItem $ HoursField Hours24 (PadWith '0')
                        ]
        ]
  suite "Parse time formatters" do
    void $ for cases \(Tuple fmt expected) -> do
      test (fmt <> " as time") do
        Assert.equal
          (Right expected)
          (parseTimeFormat fmt)
      test (fmt <> " as datetime") do
        Assert.equal
          (Right $ map TimeField <$> expected)
          (parseDateTimeFormat fmt)
      test (fmt <> " as date") do
        Assert.equal
          true
          (isLeft $ parseDateFormat fmt)

dateParserSuite :: forall e. TestSuite e
dateParserSuite = do
  let cases =
        [ Tuple "%D" [ FormatItem $ MonthNumberField (PadWith '0')
                     , Literal "/"
                     , FormatItem $ DayField (PadWith '0')
                     , Literal "/"
                     , FormatItem $ YearField Abbreviated (PadWith '0')
                     ]
        , Tuple "%F" [ FormatItem $ YearField Full NoPadding
                     , Literal "-"
                     , FormatItem $ MonthNumberField (PadWith '0')
                     , Literal "-"
                     , FormatItem $ DayField (PadWith '0')
                     ]

        , Tuple "%Y" [ FormatItem $ YearField Full NoPadding ]
        , Tuple "%y" [ FormatItem $ YearField Abbreviated (PadWith '0') ]

        , Tuple "%B" [ FormatItem $ MonthNameField Full ]
        , Tuple "%b" [ FormatItem $ MonthNameField Abbreviated ]
        , Tuple "%h" [ FormatItem $ MonthNameField Abbreviated ]
        , Tuple "%m" [ FormatItem $ MonthNumberField (PadWith '0') ]

        , Tuple "%d" [ FormatItem $ DayField (PadWith '0') ]
        , Tuple "%e" [ FormatItem $ DayField (PadWith ' ') ]

        , Tuple "%a" [ FormatItem $ WeekdayNameField Abbreviated ]
        , Tuple "%A" [ FormatItem $ WeekdayNameField Full ]
        ]
  suite "Parse date formatters" do
    void $ for cases \(Tuple fmt expected) -> do
      test (fmt <> " as date") do
        Assert.equal
          (Right expected)
          (parseDateFormat fmt)
      test (fmt <> " as datetime") do
        Assert.equal
          (Right $ map DateField <$> expected)
          (parseDateTimeFormat fmt)
      test (fmt <> " as time") do
        Assert.equal
          true
          (isLeft $ parseTimeFormat fmt)
