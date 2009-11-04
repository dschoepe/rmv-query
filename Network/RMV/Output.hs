{-# LANGUAGE NamedFieldPuns, RecordWildCards, ViewPatterns #-}

module Network.RMV.Output (ppTime,ppResult,ppRoute) where

import Network.RMV.Types

import Control.Monad
import Data.List

ppTime :: Int -> String
ppTime mins = pad (mins `div` 60) ++ ":" ++ pad (mins `mod` 60)
    where pad (show -> m) = replicate (2 - length m) '0' ++ m

ppRoute :: RouteInfo -> String
ppRoute (RouteInfo {..}) =
    intercalate " " [ppTime riDuration,"|",riStartTime,riStartPoint
                    ,"-<",riLine,">-",riEndPoint,riEndTime]

ppResult :: [[RouteInfo]] -> IO ()
ppResult = mapM_ printDetails . zip [1..]
    where printDetails (n,rs) = do
            putStrLn ("Route "++show n++":")
            mapM_ printRoute rs
            when (length rs > 1) $
                 putStrLn $ "Total duration: " ++ ppTime (sum . map riDuration $ rs)
          printRoute = putStrLn . ppRoute
