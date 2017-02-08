module Data.DateTime.Format.Parse
where

import Prelude
import Data.DateTime.Format.Field
import Data.DateTime.Format.FormatSpec
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.String as P
import Data.String (fromCharArray)
import Data.Array as Array
import Data.Foldable (fold)
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
    'y' -> pure [FormatItem $ YearField Abbreviated (fromMaybe (PadWith '0') padMay)]
    'Y' -> pure [FormatItem $ YearField Full (fromMaybe NoPadding padMay)]
    -- TODO: more specifiers
    _ -> P.fail $ "Invalid date format specifier " <> show c

mkTimeField :: Maybe Padding -> Char -> P.Parser String (Array (FormatItem TimeField))
mkTimeField padMay c =
  case c of
    'H' -> pure [FormatItem $ HoursField Hours24 (fromMaybe (PadWith '0') padMay)]
    -- TODO: more specifiers
    _ -> P.fail $ "Invalid time format specifier " <> show c

mkDateTimeField :: Maybe Padding
                -> Char
                -> P.Parser String (Array (FormatItem DateTimeField))
mkDateTimeField padMay c =
      (map (map TimeField) <$> mkTimeField padMay c)
  <|> (map (map DateField) <$> mkDateField padMay c)
  <|> (P.fail $ "Invalid date/time format specifier " <> show c)
