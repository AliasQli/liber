module Encode where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List
import Data.Word8

word8s = [toEnum 0 ..] :: [Word8]

encodeTable = reverse word8s

decodeTable = snd <$> sort (zip encodeTable word8s)

encode = B.map ((encodeTable !!) . fromEnum)

decode = B.map ((decodeTable !!) . fromEnum)
