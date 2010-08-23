{-# LANGUAGE NamedFieldPuns, RecordWildCards, ViewPatterns #-}

module Network.RMV.Output (ppTime,ppResult,ppRoute) where

import Network.RMV.Types

import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.List
import Data.Maybe
import Text.Template

ppTime :: Int -> String
ppTime mins = pad (mins `div` 60) ++ ":" ++ pad (mins `mod` 60)
    where pad (show -> m) = replicate (2 - length m) '0' ++ m

ppRoute :: Options -> RouteInfo -> String
ppRoute (Options{..}) (RouteInfo {..}) =
    map (toEnum . fromEnum) . B.unpack . substitute (fromString opTemplate) .
    M.fromList . map (fromString *** fromString) $
               [ ("duration", maybe "     " ppTime riDuration)
               , ("start_time", fromMaybe "     " riStartTime)
               , ("start", riStartPoint)
               , ("end", riEndPoint)
               , ("line", riLine)
               , ("end_time", fromMaybe "" riEndTime)
               , ("newline","\n")
               ]
    where fromString = B.pack . map (toEnum . fromEnum)

ppResult :: Options -> [[RouteInfo]] -> IO ()
ppResult opts = mapM_ printDetails . zip [1..]
    where printDetails (n,rs) = do
            putStrLn ("Route "++show n++":")
            mapM_ printRoute rs
            when (length rs > 1) $
                 let totDur = fmap (ppTime . sum) . sequence . map riDuration $ rs
                 in maybe (return ()) (putStrLn . ("Total duration: "++)) totDur
          printRoute = putStrLn . ppRoute opts
