{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where
import Data.Scientific
import Network.Wreq
import Control.Lens
import Data.Text (Text)
import System.IO
import Brick.Widgets.List
import Brick.Widgets.Edit
import Control.Concurrent
import Graphics.Vty

type Playlist = (Text, Scientific) -- all user playlists (name, id)
type AlbumDetail = (Text, Scientific) -- detail of every playlist (name, id)

data Layout = MainLayout | PlayListLayout | PlayListDetailLayout | LoginLayout

data Log = Log {
    _option :: Options,
    _userId :: Maybe Scientific,
    _playList :: [Playlist],
    _currentList :: [AlbumDetail]
}

makeLenses ''Log

-- control mplayer/monitor state
data MpObject = MpObject {
    _mpsend :: Handle,
    _mprecv :: Handle,
    _mperr :: Handle,
    _mptimePos :: Maybe Double,
    _mppercentPos :: Maybe Int,
    _mptimeLength :: Maybe Double,
    _mpupdaterId :: ThreadId,
    _mpcurrentSong :: Maybe String
}

makeLenses ''MpObject

data St = St {
    _stlog :: Log,
    _stcurrentLayout :: Layout,
    _stmain :: List Text Text,
    _stplaylist :: List Text Playlist,
    _stalbumDetail :: List Text AlbumDetail,
    _stmplayer :: Maybe (MVar MpObject),
    _stisplaying :: Bool,
    _stupdaterId :: [ThreadId],
    _sttimeline :: (Double, Double),
    _stfilenameMap :: [(String, String)],
    _stcurrentSong :: Maybe String,
    _stflag :: Bool, -- indicate if starting to play first song
    _steditlogin :: (Editor String Text, Editor String Text),
    _steditloginselect :: Int, -- 0 -> account, 1 -> password, 2,3 -> confirm/cancel
    _stlogined :: Bool,
    _stloginfailed :: Bool,
    _stcachefile :: FilePath
}

makeLenses ''St

data CustomEvent = Ev Event | UpdateTimeline | UpdateSong

