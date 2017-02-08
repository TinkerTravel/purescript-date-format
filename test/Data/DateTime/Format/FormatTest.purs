module Data.DateTime.Format.FormatTest
where

import Prelude
import Test.Unit (TestSuite, Test, test, suite, failure)
import Test.Unit.Assert (assert)
import Test.Unit.Assert as Assert
import Data.Array as Array
import Data.Either (Either (..))
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Enum (toEnum)
import Data.DateTime.Format
import Data.Date (canonicalDate, Month (..), Year, Day, Weekday (..))
import Data.Time (Time (..), Hour, Minute, Second, Millisecond)
import Data.DateTime (DateTime (..))
import Data.Tuple (Tuple (..))
import Data.Traversable (for, sequence)
import Data.Maybe (maybe)

mkYear :: Int -> Year
mkYear = toEnum >>> fromMaybe bottom

mkDay :: Int -> Day
mkDay = toEnum >>> fromMaybe bottom

formatSuite :: forall e. TestSuite e
formatSuite = do
  let sampleDT =
        DateTime
          <$> (canonicalDate <$> toEnum 2017
                             <*> toEnum 2
                             <*> toEnum 7)
          <*> (Time <$> toEnum 13
                    <*> toEnum 5
                    <*> toEnum 1
                    <*> toEnum 456)
  let cases =
        [ Tuple "" ""
        , Tuple "%Y-%m-%d %H:%M:%S" "2017-02-07 13:05:01"
        , Tuple "%A" "Tuesday"
        ]
  suite "Formatter test" do
    void $ for cases \(Tuple fmt expected) -> do
      let testName =
            if fmt == ""
              then "<empty format>"
              else show fmt <> " -> " <> show expected
      test testName do
        Assert.equal
          (Right expected)
          (maybe (Left "Invalid sample date") (formatDateTime fmt) $ sampleDT)
