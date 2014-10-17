-- Copyright © 2014 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- Decode quoted-printable text

import Data.ByteString.Lazy as B hiding (getContents)
import Data.Char (digitToInt, isHexDigit, ord)
import Data.Word

data Accum = Accum { code :: ByteString, remaining :: Int }

clear :: Accum
clear = Accum B.empty 0

leadingOnes :: Int -> Int
leadingOnes x
    | x < 0x80 = 0
    | otherwise = 1 + leadingOnes ((x - 0x80) * 2)

byte :: Char -> Word8
byte c = fromIntegral $ ord c

decodeChars :: Accum -> String -> ByteString
decodeChars _ "" = B.empty
decodeChars accum ('=' : '\n' : cs) = decodeChars accum cs
decodeChars accum ('=' : '\r' : '\n' : cs) = decodeChars accum cs
decodeChars accum ('=' : d1 : d2 : cs) | isHexDigit d1 && isHexDigit d2 =
  let
      thisDigit = 0x10 * digitToInt d1 + digitToInt d2
  in
    if thisDigit < 0x80
    then fromIntegral thisDigit `B.cons` decodeChars clear cs
    else
        let
            allDigits = code accum `B.snoc` fromIntegral thisDigit
            r = remaining accum
        in
          if thisDigit < 0xc0
          then if r == 0
               then allDigits `B.append` decodeChars clear cs
               else decodeChars (Accum allDigits (r - 1)) cs
          else decodeChars (Accum
                            (B.singleton $ fromIntegral thisDigit)
                            (leadingOnes thisDigit - 2)) cs
decodeChars _ (c : cs) = byte c `B.cons` decodeChars clear cs

main :: IO ()
main = getContents >>= B.putStr . decodeChars clear
