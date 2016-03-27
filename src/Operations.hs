{-# LANGUAGE OverloadedStrings #-}
module Operations where

import qualified Control.Monad.Trans.State as ST
import Control.Monad.Trans (lift)
import Network.Wreq
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import Data.Vector (fromList)
{-import qualified Data.ByteString.Char8 as BS-}
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Hash.MD5
import Data.Text (unpack)
import Data.Scientific

import Talker
import Constant
import Types

-- TODO: handle password mismatch and code 415 captcha
loginPhone :: String -> String -> ST.StateT Log IO Bool
loginPhone phone password = do
    opt <- ST.gets _option
    r <- lift $ postToServer loginPhoneUrl opt v
    if r ^? responseBody . key "code" . _Number == Just 200
        then ST.modify (m r) >> return True
        else return False where
    v = object ["phone" .= phone,
                "password" .= md5s (Str password),
                "rememberLogin" .= Bool True]
    m r l = l & option . cookies ?~ r ^. responseCookieJar
            & userId .~ r ^? responseBody . key "account" . key "id" . _Number

login :: String -> String -> ST.StateT Log IO Bool
login account password = do
    opt <- ST.gets _option
    r <- lift $ postToServer loginUrl opt v
    if r ^? responseBody . key "code" . _Number == Just 200
        then ST.modify (m r) >> return True
        else return False where
    v = object ["username" .= account,
                "password" .= md5s (Str password),
                "rememberLogin" .= Bool True]
    m r l = l & option . cookies ?~ r ^. responseCookieJar
            & userId .~ r ^? responseBody . key "account" . key "id" . _Number

getUserPlaylist :: ST.StateT Log IO [Playlist]
getUserPlaylist = do
    opt <- ST.gets _option
    Just uid <- ST.gets _userId
    r <- lift $ postToServer userSongListUrl opt (v uid)
    let list = r ^.. responseBody . key "playlist" . _Array . traverse
            . to ( \o -> (o ^?! key "name" . _String, o ^?! key "id" . _Number))
    ST.modify (playList .~ list)
    return list where v uid = object ["uid" .= uid, "limit" .= Number 10]

getPlaylistDetail :: Int -> ST.StateT Log IO [AlbumDetail]
getPlaylistDetail n = do
    opt <- ST.gets _option
    pl <- ST.gets _playList
    let pick = snd $ pl !! n
    r <- lift $ postToServer playlistDetailUrl opt (v pick)
    let list = r ^.. responseBody . key "playlist" . key "tracks" . _Array
            . traverse . to (\o ->
                (o ^?! key "name" . _String, o ^?! key "id" . _Number))
    ST.modify (currentList .~ list)
    return list
        where
        v pick = object ["id" .= pick,
                    "offset" .= Number 0,
                    "total" .= Bool True,
                    "limit" .= Number 1000,
                    "n" .= Number 1000]

getMusicUrl :: Int -> ST.StateT Log IO String
getMusicUrl n = do
    opt <- ST.gets _option
    ad <- ST.gets _currentList
    let pick = snd $ ad !! n
    r <- lift $ postToServer queryMusicUrl opt (v pick)
    return . unpack $ r ^. responseBody . key "data" . nth 0 . key "url" . _String where
        v pick = object ["ids" .= Array (fromList [Number pick]),
                        "br" .= brH]

-- the caller garenteens start <= total
getMusicsUrl :: Int -> Int -> ST.StateT Log IO [(String, Scientific)]
getMusicsUrl start total = do
    opt <- ST.gets _option
    ad <- ST.gets _currentList
    let picks = map (\n -> snd $ ad !! n) [start..total]
    r <- lift $ postToServer queryMusicUrl opt (v picks)
    return $ r ^.. responseBody . key "data"
            . _Array . traverse . to ( \o ->
                (unpack $ o ^. key "url" . _String, o ^?! key "id" . _Number)
            ) where
        v picks = object ["ids" .= Array (fromList (Number <$> picks)),
                        "br" .= brH]


downloadMusic :: String -> ST.StateT Log IO ()
downloadMusic url = do
    opt <- ST.gets _option
    r <- lift $ getWith opt url
    lift $ LBS.writeFile "test.mp3" (r ^. responseBody)

