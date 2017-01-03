{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module UI where

import Brick
import Brick.Widgets.List
import Brick.Widgets.Edit
import Graphics.Vty
import Control.Lens
import Data.Text hiding (intercalate)
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.Monoid
import Network.Wreq
import Data.Vector as V hiding (replicate, mapM_)
import Control.Concurrent
import Data.Default
import Text.Printf
import System.IO.Temp
import System.Directory
import System.IO
import Data.Maybe
import Data.List (intercalate)
import System.FilePath


import Constant
import Operations
import Types
import Player

padLeft' :: Int -> Widget n -> Widget n
padLeft' = padLeft . Pad

padTop' :: Int -> Widget n -> Widget n
padTop' = padTop . Pad

playerUI :: St -> Widget n
playerUI st =
    let b = st ^. stflag
    in if b then playerUI' st else emptyWidget

doubleToTime :: Double -> String
doubleToTime d =
    let m = truncate (d / 60) :: Int
        s = truncate (d - realToFrac (m * 60)) :: Int
    in printf "%02d" m <> ":" <> printf "%02d" s

playerUI' :: St -> Widget n
playerUI' st =
    let (a, b) = st ^. sttimeline
        w1 =
            let played = truncate (a / b * 50)
                played' = if played > 50 then 50 else played
                unplayed = 50 - played
                pb1 = withAttr progressBarAttr $ str $ Prelude.replicate played' ' '
                pb2 = withAttr progressBarInAttr $ str $ Prelude.replicate unplayed ' '
                pb = pb1 <+> pb2
            in str "[" <+> pb <+> str "]"
        w2 = str $ doubleToTime a <> "/" <> doubleToTime b
        filename = withAttr songNameAttr $ str $
            fromMaybe "" (st ^. stcurrentSong)
        pa = withAttr pausedAttr $ if st ^. stisplaying then str "" else str "(paused)"
    in padLeft' 6 $ (str " " <=> (w1 <+> str " " <+> w2) <=> str " ") <=>
        padLeft' 5 (filename <+> str " " <+> pa)

renderMainLayout :: St -> [Widget Text]
renderMainLayout st =
    let ls = st ^. stmain
    in [padTop' 2 (vLimit 18 $ vBox [renderList rend True ls]) <=>
        playerUI st] where
        rend b t = if b
                     then padLeft' 10 $ withAttr listSelectedAttr $ txt t
                     else padLeft' 9 $ withAttr listAttr $ txt t

renderPlaylistLayout :: St -> [Widget Text]
renderPlaylistLayout st =
    let ls'' = fst <$> (st ^. stplaylist)
        ls' = ls'' ^. listElementsL
        ls = V.zipWith (\a b -> pack (show b) <> ". " <> a)
                ls' (fromList [0..V.length ls' - 1] :: Vector Int)
    in [vBox [padTop' 2 $ vLimit 18 $ vBox
        [renderList rend True $ ls'' & listElementsL .~ ls], playerUI st]] where
        rend b t = if b  then padLeft' 10 $ withAttr listSelectedAttr $ txt t
                            else padLeft' 9 $ withAttr listAttr $ txt t

renderPlaylistDetailLayout :: St -> [Widget Text]
renderPlaylistDetailLayout st =
    let ls'' = fst <$> (st ^. stalbumDetail)
        ls' = ls'' ^. listElementsL
        ls = V.zipWith (\a b -> pack (show b) <> ". " <> a)
                ls' (fromList [0..V.length ls' - 1] :: Vector Int)
    in [vBox [padTop' 2 $ vLimit 18 $ vBox
        [renderList rend True $ ls'' & listElementsL .~ ls], playerUI st]] where
        rend b t = if b  then padLeft' 10 $ withAttr listSelectedAttr $ txt t
                            else padLeft' 9 $ withAttr listAttr $ txt t

renderLoginLayout :: St -> [Widget Text]
renderLoginLayout st = [padAll 5 w] where
    w = waccount <=> str " " <=> str " " <=> wpassword <=> str " " <=>
        padTop' 1 (padLeft' 8 $ wconfirm <+> str "        " <+> wcancel)
        <=> padTop' 1wfailmessage
    waccount = vLimit 1 $ wastr <+> renderEditor True (fst $ st ^. steditlogin)
    wpassword = vLimit 1 $ wapwd <+> renderwpwd
    renderwpwd = str $ Prelude.replicate (Prelude.length . Prelude.head
                    . getEditContents . snd $ st ^. steditlogin) '*'
    selected = st ^. steditloginselect
    wastr' = str "Account: "
    wapwd' = str "Password: "
    wconfirm' = str "Confirm"
    wcancel' = str "Cancel"
    choices = [wastr', wapwd', wconfirm', wcancel']
    [wastr, wapwd, wconfirm, wcancel] =
        choices & ix selected %~ withAttr editSelected
    wfailmessage = if st ^. stloginfailed then str "" else
        withAttr pausedAttr (str "login failed, pls try again.")

monadSet :: (Ord n, Show n) => St -> Lens' St (List n e) -> Event -> EventM n (Next St)
monadSet st len ev = do
    new <- handleListEvent ev (st ^. len)
    continue $ st & len .~ new

handleMainLayoutEvent :: St -> CustomEvent -> EventM Text (Next St)
handleMainLayoutEvent st ev =
    case ev of
        Ev (EvKey (KChar 'j') []) -> monadSet st stmain (EvKey KDown [])
        Ev (EvKey (KChar 'k') []) -> monadSet st stmain (EvKey KUp [])
        Ev (EvKey (KChar 'd') [MCtrl]) -> monadSet st stmain (EvKey KPageDown [])
        Ev (EvKey (KChar 'u') [MCtrl]) -> monadSet st stmain (EvKey KPageUp [])
        Ev (EvKey (KChar 'l') []) -> -- switch from MainLayout to PlayListLayout
            case st ^. stmain . listSelectedL of
                Just 0 -> continue $ st & stcurrentLayout .~ LoginLayout
                Just 1 ->
                    if st ^. stlogined then do
                        (r, l) <- liftIO $ runStateT getUserPlaylist (st ^. stlog)
                        continue $ st & stlog .~ l & stcurrentLayout .~ PlayListLayout
                            & stplaylist .~ list "playlist" (fromList r) 1
                        else continue st
                _ -> continue st
        _ -> genericHandler st ev

handlePlaylistLayoutEvent :: St -> CustomEvent -> EventM Text (Next St)
handlePlaylistLayoutEvent st ev =
    case ev of
        Ev (EvKey (KChar 'j') []) -> monadSet st stplaylist (EvKey KDown [])
        Ev (EvKey (KChar 'k') []) -> monadSet st stplaylist (EvKey KUp [])
        Ev (EvKey (KChar 'd') [MCtrl]) -> monadSet st stplaylist (EvKey KPageDown [])
        Ev (EvKey (KChar 'u') [MCtrl]) -> monadSet st stplaylist (EvKey KPageUp [])
        Ev (EvKey (KChar 'h') []) -> continue $ st & stcurrentLayout .~ MainLayout
        Ev (EvKey (KChar 'l') []) -> do -- switch from PlayListLayout to PlayListDetailLayout
            let selected = st ^. stplaylist . listSelectedL
            case selected of
                Just n -> do
                    (r, l) <- liftIO $ runStateT (getPlaylistDetail n) (st ^. stlog)
                    continue $ st & stlog .~ l & stcurrentLayout .~ PlayListDetailLayout
                        & stalbumDetail .~ list "albumdetail" (fromList r) 1
                Nothing -> continue st
        _ -> genericHandler st ev

handlePlaylistDetailLayoutEvent :: St -> CustomEvent -> EventM Text (Next St)
handlePlaylistDetailLayoutEvent st ev =
    case ev of
        Ev (EvKey (KChar 'j') []) -> monadSet st stalbumDetail (EvKey KDown [])
        Ev (EvKey (KChar 'k') []) -> monadSet st stalbumDetail (EvKey KUp [])
        Ev (EvKey (KChar 'd') [MCtrl]) -> monadSet st stalbumDetail (EvKey KPageDown [])
        Ev (EvKey (KChar 'u') [MCtrl]) -> monadSet st stalbumDetail (EvKey KPageUp [])
        Ev (EvKey (KChar 'h') []) -> continue $ st & stcurrentLayout .~ PlayListLayout
        Ev (EvKey (KChar 'l') []) -> do -- play music
            let selected = st ^. stalbumDetail . listSelectedL
            case selected of
                Just n -> do
                    let total = V.length $ st ^. stalbumDetail . listElementsL
                    if n <= total - 1
                      then do -- add a single song at a time
                        uri <- liftIO $ evalStateT (getMusicUrl n _currentList) (st ^. stlog)
                        exeMplayer (playMusic uri) $ st & stisplaying .~ True
                                                        & stflag .~ True
                                                        & stfilenameMap .~ [(takeFileName uri,
                                                    unpack . fst $ (st^.stlog.currentList) !! n)]
                                                        & stcurrentSongNumber .~ n
                                                        & stnextsongappended .~ False
                                                        & stlog . currentPlayingList .~
                                                            st ^. stlog . currentList
                          {- r <- liftIO $ evalStateT (getMusicsUrl n (total - 1)) -}
                              {- (st ^. stlog) -}
                          {- let urls = fst <$> r -}
                              {- ids = snd <$> r -}
                          {- tdir <- liftIO getTemporaryDirectory -}
                          {- (fp, h) <- liftIO $ openTempFile tdir "mplayer.playlist" -}
                          {- liftIO $ hPutStrLn h (intercalate "\n" urls) >> hFlush h -}
                          {- let fns = Prelude.map takeFileName urls -}
                              {- albummap = (\(a,b) -> (b, a)) <$> st^.stlog.currentList -}
                              {- songs = Prelude.map -}
                                  {- (\i -> unpack . fromJust $ lookup i albummap) ids -}
                          {- result <- exeMplayer (playMusicFile fp) -}
                              {- (st & stisplaying .~ True -}
                                  {- & stfilenameMap .~ Prelude.zip fns songs -}
                                  {- & stflag .~ True) -}
                          {- liftIO $ threadDelay 300000 >> removeFile fp -}
                          {- return result -}
                      else continue st
                Nothing -> continue st
        _ -> genericHandler st ev

handleLoginLayoutEvent :: St -> CustomEvent -> EventM Text (Next St)
handleLoginLayoutEvent st ev = do
    let (a, b) = st ^. steditlogin
    case ev of
        Ev (EvKey KEsc []) -> continue $ st & stcurrentLayout .~ MainLayout
                        & stloginfailed .~ True & steditlogin .~ emptyEditPair
        Ev (EvKey (KChar '\t') []) -> continue $
                            st & steditloginselect %~ (\i -> (i + 1) `rem` 4)
        Ev (EvKey KBackTab []) -> continue $
                            st & steditloginselect %~ (\i -> (i - 1) `mod` 4)
        Ev (EvKey KEnter []) ->
            case st ^. steditloginselect of
                0 -> continue $ st & steditloginselect .~ 1
                1 -> performLogin st
                2 -> performLogin st
                _ -> continue $ st & stcurrentLayout .~ MainLayout
                        & stloginfailed .~ True & steditlogin .~ emptyEditPair
        Ev event ->
            case st ^. steditloginselect of
                0 -> do
                    newa <- handleEditorEvent event a
                    continue $ st & steditlogin .~ (newa, b)
                1 -> do
                    newb <- handleEditorEvent event b
                    continue $ st & steditlogin .~ (a, newb)
                _ -> continue st
        _ -> continue st

genericHandler :: St -> CustomEvent -> EventM Text (Next St)
genericHandler st ev =
    case ev of
        Ev (EvKey (KChar 'q') []) -> halt st
        Ev (EvKey KEsc []) -> halt st
        Ev (EvKey KRight []) ->
               if st ^. stisplaying then exeMplayer (seekRelative 15) st
                   else continue st
        Ev (EvKey KLeft []) ->
               if st ^. stisplaying then exeMplayer (seekRelative (-15)) st
                   else continue st
        Ev (EvKey KUp []) ->
               if st ^. stisplaying then exeMplayer (addVolume 1) st
                   else continue st
        Ev (EvKey KDown []) ->
            if st ^. stisplaying then exeMplayer (addVolume (-1)) st
                else continue st
        Ev (EvKey (KChar 'p') []) ->
            if st ^. stisplaying then exeMplayer pauseMplayer
                (st & stisplaying .~ False)
                else exeMplayer pauseMplayer (st & stisplaying .~ True)
        UpdateTimeline -> --carefully update time line infomation
            if st ^. stisplaying then updateTime st
                else continue st
        UpdateSong ->
            if st ^. stisplaying then updateSong st
                else continue st
        _ -> continue st

performLogin :: St -> EventM Text (Next St)
performLogin st = do
    let account = Prelude.head $ getEditContents (fst $ st ^. steditlogin)
        password = Prelude.head $ getEditContents (snd $ st ^. steditlogin)
    (result, ok) <- tryPhoneLogin account password st
    if ok then liftIO (writeCache result)
            >> continue (result & stcurrentLayout .~ MainLayout
                                & stlogined .~ True)  else do
        (result', ok') <- tryLogin account password st
        if ok' then liftIO (writeCache result) >>
            continue (result' & stcurrentLayout .~ MainLayout
                            & stlogined .~ True) else
            continue $ st & stloginfailed .~ False & steditlogin .~ emptyEditPair

writeCache :: St -> IO ()
writeCache st = withFile (st ^. stcachefile) WriteMode $ \h -> do
    hPrint h $ fromJust $ st ^. stlog . userId
    hPrint h $ fromJust $ st ^. stlog . option . cookies



tryPhoneLogin :: String -> String -> St -> EventM n (St, Bool)
tryPhoneLogin phone pwd st = do
    (r, l) <- liftIO $ runStateT (loginPhone phone pwd) (st ^. stlog)
    if r then return (st&stlog .~ l, True) else return (st, False)

tryLogin :: String -> String -> St -> EventM n (St, Bool)
tryLogin account pwd st = return (st, False)

updateTime :: St -> EventM n (Next St)
updateTime st = case st ^. stmplayer of
    Nothing -> error "this should not happen!!!"
    Just mp -> do
        t <- liftIO $
            ( \a b -> do
                tp <- a
                tl <- b
                return (tp, tl) ) <$> getTimePosition mp <*> getTimeLength mp
        case t of
            Nothing -> continue st
            Just (tp', tl') -> do
                let flag | tp' / tl' > 0.9 = if st ^. stnextsongappended
                                                then 0 -- do nothing
                                                else 1 -- append song
                         | st ^. stnextsongappended = 2 -- appended song playing
                                                        -- clear it
                         | otherwise = 0                -- do nothing
                case flag of
                    0 -> continue $ st & sttimeline .~ (tp', tl')
                    1 -> do
                        let n = st ^. stcurrentSongNumber
                        if n < Prelude.length (st ^. stlog . currentList) - 1
                          then do
                            uri <- liftIO $ evalStateT
                                (getMusicUrl (n+1) _currentPlayingList) (st ^. stlog)
                            let newTuple = (takeFileName uri,
                                    unpack . fst $ (st^.stlog.currentPlayingList) !! (n+1))
                            exeMplayer (appendMusic uri) $ st & stisplaying .~ True
                                                & stflag .~ True
                                                & stfilenameMap .~
                                                    [newTuple, Prelude.head $
                                                        st^.stfilenameMap]
                                                & stcurrentSongNumber .~ (n + 1)
                                                & sttimeline .~ (tp', tl')
                                                & stnextsongappended .~ True
                          else continue $ st & sttimeline .~ (tp', tl')
                                            & stnextsongappended .~ True
                    2 -> continue $ st & sttimeline .~ (tp', tl')
                                    & stnextsongappended .~ False


updateSong :: St -> EventM n (Next St)
updateSong st = case st ^. stmplayer of
    Nothing -> error "this should not happend!!!!"
    Just mp -> do
        fn <- liftIO (getFileName mp)
        case fn of
            Nothing -> continue st
            Just fn' -> continue $ st & stcurrentSong .~ lookup fn' (st^.stfilenameMap)

getMplayerProperty :: (MVar MpObject -> IO a) -> St -> EventM n a
getMplayerProperty action st = do
    let mp = st ^. stmplayer
    case mp of
        Just mp' -> liftIO $ action mp'
        Nothing -> error "invalid calling"

exeMplayer :: (MVar MpObject -> IO ()) -> St -> EventM n (Next St)
exeMplayer action st = do
    let mp = st ^. stmplayer
    case mp of
        Just mp' -> liftIO $ action mp'
        Nothing -> return ()
    continue st

uiAppDraw :: St -> [Widget Text]
uiAppDraw st =
    case st ^. stcurrentLayout of
        MainLayout -> renderMainLayout st
        PlayListLayout -> renderPlaylistLayout st
        PlayListDetailLayout -> renderPlaylistDetailLayout st
        LoginLayout -> renderLoginLayout st

uiAppHandleEvent :: St -> CustomEvent -> EventM Text (Next St)
uiAppHandleEvent st ev =
    case st ^. stcurrentLayout of
        MainLayout -> handleMainLayoutEvent st ev
        PlayListLayout -> handlePlaylistLayoutEvent st ev
        PlayListDetailLayout -> handlePlaylistDetailLayoutEvent st ev
        LoginLayout -> handleLoginLayoutEvent st ev

theApp :: App St CustomEvent Text
theApp = App {
    appDraw = uiAppDraw,
    appChooseCursor = neverShowCursor,
    appHandleEvent = uiAppHandleEvent,
    appStartEvent = return,
    appAttrMap = const $ attrMap defAttr customAttrMap,
    appLiftVtyEvent = Ev
}

progressBarAttr :: AttrName
progressBarAttr = "progressbar"

progressBarInAttr :: AttrName -- progressbar-incomplete
progressBarInAttr = "progressbarin"

songNameAttr :: AttrName
songNameAttr = "songname"

pausedAttr :: AttrName
pausedAttr = "paused"

editSelected :: AttrName
editSelected = "focused"

customAttrMap :: [(AttrName, Attr)]
customAttrMap = [(listSelectedAttr, fg brightGreen `withStyle` bold),
                (listAttr, fg brightYellow),
                (progressBarAttr, bg brightBlack),
                (progressBarInAttr, fg brightBlack),
                (songNameAttr, fg brightCyan `withStyle` bold),
                (pausedAttr, fg red),
                (editSelected, fg brightGreen `withStyle` bold)]

