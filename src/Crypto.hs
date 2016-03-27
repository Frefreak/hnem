module Crypto where

import Crypto.Cipher
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.ByteString.Base64 (encode)
import Data.Hex (hex)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack, unpack)
import Numeric (showHex, readHex)

initAES128 :: ByteString -> AES128
initAES128 = either (error . show) cipherInit . makeKey

cbcEncryption :: AES128 -> ByteString -> ByteString -> ByteString
cbcEncryption ctx ivRaw = cbcEncrypt ctx inivec
  where inivec = fromMaybe (error "invalid IV") (makeIV ivRaw)

aesEncrypt :: ByteString -> ByteString -> ByteString -> ByteString
aesEncrypt text inivec secKey =
    encode $ cbcEncryption (initAES128 secKey) inivec text' where
        len = BS.length text
        pad = 16 - len `rem` 16
        text' = text `BS.append` BS.replicate pad (fromIntegral pad)

rsaEncrypt :: ByteString -> Integer -> ByteString -> ByteString
rsaEncrypt text pbk modu =
    let [(i, _)] = readHex . unpack . hex $ BS.reverse text
        [(m, _)] = readHex .unpack $ modu
        rs = i ^ pbk `rem` m :: Integer
        result = pack $ showHex rs ""
    in if BS.length result < 256 then
        BS.replicate (256 - BS.length result) 48 `BS.append` result else
        result

