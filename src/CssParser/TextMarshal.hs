module CssParser.TextMarshal
  ( module CssParser.TextMarshal
  , readEither
  ) where

import CssParser.Prelude
import Data.Text.Lazy qualified as L
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Text.Read (readEither)

numToText :: Integral a => a -> LText
numToText = toLazyText . decimal

showHex :: Int -> ShowS
showHex = go (6 :: Int)
  where
    go 0 _ s = s
    go k n rs = go (k - 1) q (intToDigit r : rs)
      where
        ~(q, r) = quotRem n 16

encodeStringChar :: Char -> LText
encodeStringChar c
  | c `notElem` ("\"\\\n\t\r\f" :: String) = L.singleton c
  | otherwise = cons '\\' (L.pack (showHex (ord c) ""))

encodeStringLiteral :: Text -> LText
encodeStringLiteral t =
  cons c (snoc (L.concatMap encodeStringChar $ fromStrict t) c)
  where
    c = '"'

readHex :: String -> Either String Integer
readHex = \case
  [a, b, c] -> readEither ['0', 'x', a, a, b, b, c, c]
  o -> readEither $ '0':'x':o
