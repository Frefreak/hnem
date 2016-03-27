{-# LANGUAGE OverloadedStrings #-}
module Talker where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as LBS (toStrict, ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS (putStrLn)
import Network.Wreq
import qualified Network.Wreq.Session as S
import Control.Lens hiding ((.=))
import Data.Aeson
import System.Random
import Numeric (showHex)

import Crypto
import Constant

createSecretKey :: Int -> IO ByteString
createSecretKey n = do
    g <- newStdGen
    let ns = map (`mod` 16) (take n $ randoms g :: [Int])
    return . pack . concat $ map (`showHex` "") ns

neteaseParams' :: ByteString -> ByteString -> ByteString
neteaseParams' text = aesEncrypt (aesEncrypt text iv nonce) iv

neteaseParams :: ByteString -> IO (ByteString, ByteString)
neteaseParams text = do
    encSecKey <- createSecretKey 16
    return (neteaseParams' text encSecKey, encSecKey)

composeText :: LBS.ByteString -> IO [FormParam]
composeText text = do
    (params', key') <- neteaseParams $ LBS.toStrict text
    let encSecKey = rsaEncrypt key' pubKey modulus
    return ["params" := params', "encSecKey" := encSecKey]

postToServer :: ToJSON r => String -> Options -> r -> IO (Response LBS.ByteString)
postToServer url opts text = do
    body <- composeText . encode $ text
    postWith opts url body

getFromUser :: String -> Options -> IO (Response LBS.ByteString)
getFromUser url opts = getWith opts url
