-- Copyright © 2014 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- Decode quoted-printable text

import Data.ByteString.Lazy as B hiding (getContents)
import Data.Char (digitToInt, isHexDigit, ord)
import Data.Word

byte :: Char -> Word8
byte c = fromIntegral $ ord c

decodeChars :: String -> ByteString
decodeChars "" = B.empty
decodeChars ('=' : '\n' : cs) = decodeChars cs
decodeChars ('=' : '\r' : '\n' : cs) = decodeChars cs
decodeChars ('=' : d1 : d2 : cs) | isHexDigit d1 && isHexDigit d2 =
  let
      thisDigit = 0x10 * digitToInt d1 + digitToInt d2
  in
    fromIntegral thisDigit `B.cons` decodeChars cs
decodeChars (c : cs) = byte c `B.cons` decodeChars cs

main :: IO ()
main = getContents >>= B.putStr . decodeChars
