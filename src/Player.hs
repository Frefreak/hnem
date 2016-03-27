{-# LANGUAGE RankNTypes #-}
module Player where
import System.Process
import System.IO
import Control.Lens
import Control.Concurrent
import Data.List

import Types
import Constant

startMplayer :: IO (MVar MpObject)
startMplayer = do
    (Just i, Just o, Just e, _) <- createProcess_ ""  $ (shell "mplayer -quiet -slave -input nodefault-bindings -noconfig all -idle") {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
    hSetBuffering i NoBuffering
    hSetBuffering o NoBuffering
    hSetBuffering e NoBuffering
    {-mp <- newMVar def { _mpsend = i, _mprecv = o, _mperr = e }-}
    mp <- newMVar initialMpObject { _mpsend = i, _mprecv = o, _mperr = e }
    tid <- forkIO (updateOutput mp)
    modifyMVar_ mp (return . (mpupdaterId .~ tid))
    return mp

modifyValue :: String -> String -> Lens' MpObject a -> MVar MpObject -> (String -> a) -> IO ()
modifyValue prefix input lns mp trans = do
    let input' = drop (length prefix) input
    modifyMVar_ mp (return . (lns .~ trans input'))

updateOutput :: MVar MpObject -> IO ()
updateOutput mp = do
    mp' <- readMVar mp
    input <- hGetLine $ mp' ^. mprecv
    case () of _
                | "ANS_TIME_POSITION=" `isPrefixOf` input -> modifyValue
                    "ANS_TIME_POSITION=" input mptimePos mp (Just . read)
                | "ANS_PERCENT_POSITION=" `isPrefixOf` input -> modifyValue
                    "ANS_PERCENT_POSITION=" input mppercentPos mp (Just . read)
                | "ANS_LENGTH=" `isPrefixOf` input -> modifyValue
                    "ANS_LENGTH=" input mptimeLength mp (Just . read)
                | "ANS_FILENAME=" `isPrefixOf` input -> modifyValue
                    "ANS_FILENAME=" input mpcurrentSong mp (Just . tail . init)
                | otherwise -> return ()
    updateOutput mp

sendCommand :: String -> MVar MpObject -> IO ()
sendCommand cmd mp = do
    mp' <- readMVar mp
    hPutStrLn (mp' ^. mpsend) cmd

playMusic :: String -> MVar MpObject -> IO ()
playMusic uri = sendCommand ("loadfile " ++ uri)

playMusicFile :: FilePath -> MVar MpObject -> IO ()
playMusicFile fp = sendCommand ("loadlist " ++ fp)

appendMusic :: String -> MVar MpObject -> IO ()
appendMusic uri = sendCommand ("loadfile " ++ uri ++ " 2")

appendMusicFile :: FilePath -> MVar MpObject -> IO ()
appendMusicFile fp = sendCommand ("loadlist " ++ fp ++ " 1")

seekRelative :: Int -> MVar MpObject -> IO ()
seekRelative n = sendCommand ("seek " ++ show n ++ " 0")

addVolume :: Int -> MVar MpObject -> IO ()
addVolume n = sendCommand ("volume " ++ show n)

pauseMplayer :: MVar MpObject -> IO ()
pauseMplayer = sendCommand "pause"

getTimePosition :: MVar MpObject -> IO (Maybe Double)
getTimePosition mp = do
    sendCommand "pausing_keep_force get_time_pos" mp
    mp' <- readMVar mp
    return $ mp' ^. mptimePos

getPercentPosition :: MVar MpObject -> IO (Maybe Int)
getPercentPosition mp = do
    sendCommand "pausing_keep_force get_percent_pos" mp
    {-threadDelay 100000-}
    mp' <- readMVar mp
    return $ mp' ^. mppercentPos

getTimeLength :: MVar MpObject -> IO (Maybe Double)
getTimeLength mp = do
    sendCommand "pausing_keep_force get_time_length" mp
    mp' <- readMVar mp
    return $ mp' ^. mptimeLength

getFileName :: MVar MpObject -> IO (Maybe String)
getFileName mp = do
    sendCommand "pausing_keep_force get_file_name" mp
    mp' <- readMVar mp
    return $ mp' ^. mpcurrentSong

terminateMplayer :: MVar MpObject -> IO ()
terminateMplayer mp = do
    mp' <- readMVar mp
    killThread $ mp' ^. mpupdaterId
    sendCommand "quit" mp
