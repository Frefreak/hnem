{-# LANGUAGE OverloadedStrings #-}
module Fakemain where

import Brick.Main
import Brick.BChan
import Control.Concurrent
import Control.Monad
import Graphics.Vty
import Data.Default
import Network.Wreq
import Control.Lens
import System.Environment.XDG.BaseDir
import Network.HTTP.Conduit
import Data.Scientific
import Text.Read
import Data.Maybe (isJust, fromJust)
import System.Posix.Files
import System.Directory

import Player
import UI
import Types
import Constant

updateTimeLine :: IO (ThreadId, BChan CustomEvent)
updateTimeLine = do
    chan <- newBChan 100
    tid <- forkIO $ forever $ do
        writeBChan chan UpdateTimeline
        threadDelay 500000
    return (tid, chan)

main' :: IO ()
main' = do
    mp <- startMplayer
    (tid1, chan) <- updateTimeLine
    tid2 <- forkIO $ forever $ do
        writeBChan chan UpdateSong
        threadDelay 1000000
    opt <- defOpt
    cache <- getUserCacheFile "hnem" "cache"
    cacheDir <- getUserCacheDir "hnem"
    db <- doesDirectoryExist cacheDir
    b <- fileExist cache
    unless db $ createDirectory cacheDir
    unless b $ writeFile cache "this is your first login"
    (uid': cookieJar') <- lines <$> readFile cache
    let cookiejar = readMaybe (concat cookieJar') :: Maybe CookieJar
        uid = readMaybe uid' :: Maybe Scientific
        st = if isJust cookiejar && isJust uid then
                let opt' = opt & cookies .~ cookiejar
                    in initialSt & stlog . option .~ opt'
                                & stlog . userId .~ uid
                                & stlogined .~ True
                    else initialSt & stlog . option .~  opt
    void $ customMain (mkVty defaultConfig) (Just chan) theApp st
        {    _stmplayer = Just mp,
            _stupdaterId = [tid1, tid2],
            _stcachefile = cache
        }
    terminateMplayer mp
    mapM_ killThread [tid2, tid2]
