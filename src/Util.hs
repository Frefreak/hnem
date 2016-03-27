module Util where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

printList :: [(T.Text, a)] -> IO ()
printList as = printList' $ map fst as where
    printList' xs = mapM_ printTuple $ zip [0..] xs
    printTuple (index, a) = do
        putStr (show index)
        putStr ". "
        TIO.putStrLn a
