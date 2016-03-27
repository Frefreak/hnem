{-# LANGUAGE OverloadedStrings #-}
module Constant where

import Data.Aeson
import Data.ByteString (ByteString)
import Network.Wreq
import Network.HTTP.Client.TLS
import Network.HTTP.Client
import Network.Connection
import Data.Default (def)
import Data.Text (Text)
import Brick.Widgets.Core
import Brick.Widgets.List
import Brick.Widgets.Edit
import Data.Vector (fromList)
import System.IO
import Data.Time.Clock
import Control.Lens

import Types

-- encrypting constants
iv :: ByteString
iv = "0102030405060708"

pubKey :: Integer
pubKey = 0x010001

nonce :: ByteString
nonce = "0CoJUm6Qyw8W8jud"

modulus :: ByteString
modulus = "00e0b509f6259df8642dbc35662901477df22677ec152b5ff68ace615bb7b725152b3ab17a876aea8a5aa76d2e417629ec4ee341f56135fccf695280104e0312ecbda92557c93870114af6c9d05c4f7f0c3685b7a46bee255932575cce10b424d813cfe4875d3e82047b97ddef52741d546b8e289dc6935b3ece0462db0a22b8e7"

insecureSetting :: ManagerSettings
insecureSetting = mkManagerSettings
    (def { settingDisableCertificateValidation = True }) Nothing

-- urls
commentsUrl :: String
commentsUrl = "http://music.163.com/weapi/v1/resource/comments/R_SO_4_30953009/?csrf_token="

loginUrl :: String
loginUrl = "https://music.163.com/weapi/login/"

loginPhoneUrl :: String
loginPhoneUrl = "https://music.163.com/weapi/login/cellphone/"

userSongListUrl :: String
userSongListUrl = "http://music.163.com/weapi/user/playlist?csrf_token="

playlistDetailUrl :: String
playlistDetailUrl = "http://music.163.com/weapi/v3/playlist/detail?csrf_token="

queryMusicUrl :: String
queryMusicUrl = "http://music.163.com/weapi/song/enhance/player/url?csrf_token="

initialState :: Log
initialState = Log defaults Nothing [] []

initialSt :: St
initialSt = St {
    _stlog = initialState,
    _stcurrentLayout = MainLayout,
    _stmain = list "main" (fromList mainMenu) (length mainMenu),
    _stplaylist = list "playlist" mempty 0,
    _stalbumDetail = list "albumdetail" mempty 0,
    _stmplayer = Nothing,
    _stisplaying = False,
    _sttimeline = (0, 0),
    _stfilenameMap = [],
    _stcurrentSong = Nothing,
    _stflag = False,
    _stupdaterId = error "not initialized",
    _steditlogin = emptyEditPair,
    _steditloginselect = 0,
    _stlogined = False,
    _stloginfailed = True,
    _stcachefile = ""
}

emptyEditPair :: (Editor, Editor)
emptyEditPair = (editor "account" (str . concat) Nothing "",
            editor "password" (str . concat) Nothing "")

initialMpObject :: MpObject
initialMpObject = MpObject { _mpsend = stdin,
                            _mprecv = stdout,
                            _mperr = stderr,
                            _mptimePos = Nothing,
                            _mptimeLength = Nothing,
                            _mppercentPos = Nothing,
                            _mpupdaterId = error "not initialized",
                            _mpcurrentSong = Nothing}

-- BaudRate
brA, brM, brH, brL :: Value
brA = Number 64000
brM = Number 160000
brH = Number 320000
brL = Number 96000

-- Main Menu (progressively adding functions)
mainMenu :: [Text]
mainMenu = ["Login",
            "Playlist"]

defCookie :: IO Cookie
defCookie = do
    cur <- getCurrentTime
    return $ Cookie "appver"
                    "2.0.2"
                    (addUTCTime 36000 cur)
                    "music.163.com"
                    "/"
                    cur
                    cur
                    True
                    True
                    False
                    True

defOpt :: IO Options
defOpt = do
    c <- defCookie
    return $ defaults   & cookies ?~ createCookieJar [c]
                        & header "Referer" .~ ["http://music.163.com/"]
                        & manager .~ Left insecureSetting
                        {-& proxy ?~ httpProxy "localhost" 8080-}

neteaseDefaultLog :: IO Log
neteaseDefaultLog = do
    defopt <- defOpt
    return $ initialState & option .~ defopt

