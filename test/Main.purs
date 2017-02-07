module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Aff.AVar (AVAR)
import Test.Unit (Test, test, suite)
import Test.Unit.Assert (assert)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Data.Array as Array
import Data.Either (Either (..))
import Data.DateTime.Format

main :: forall e. Eff (avar :: AVAR, testOutput :: TESTOUTPUT, console :: CONSOLE | e) Unit
main = do
  runTest do
    suite "Parser" do
      test "empty format string" do
        let expected = Right []
            actual = parseDateTimeFormat ""
        Assert.equal expected actual
      test "literal format string" do
        let expected = Right [Literal "Hello"]
            actual = parseDateTimeFormat "Hello"
        Assert.equal expected actual
      test "literal percent sign" do
        let expected = Right [Literal "%"]
            actual = parseDateTimeFormat "%%"
        Assert.equal expected actual
      test "%Y as date format string" do
        let expected = Right [FormatItem (YearField Full NoPadding)]
            actual = parseDateFormat "%Y"
        Assert.equal expected actual
      test "%Y as time format string (fails)" do
        let expected = Left "Invalid time format specifier 'Y'"
            actual = parseTimeFormat "%Y"
        Assert.equal expected actual
      test "%Y as date/time format string" do
        let expected = Right [FormatItem (DateField $ YearField Full NoPadding)]
            actual = parseDateTimeFormat "%Y"
        Assert.equal expected actual
      test "%H as time format string" do
        let expected = Right [FormatItem (HoursField Hours24 (PadWith '0'))]
            actual = parseTimeFormat "%H"
        Assert.equal expected actual
      test "%H as date format string (fails)" do
        let expected = Left "Invalid date format specifier 'H'"
            actual = parseDateFormat "%H"
        Assert.equal expected actual
      test "%H as date/time format string" do
        let expected = Right [FormatItem (TimeField $ HoursField Hours24 (PadWith '0'))]
            actual = parseDateTimeFormat "%H"
        Assert.equal expected actual
