module Network.RMV.Types
    (RouteInfo(..),Options(..),defaultOptions)
        where

data RouteInfo = RouteInfo { riStartTime :: String
                           , riEndTime :: String
                           , riStartPoint :: String
                           , riLine :: String
                           , riEndPoint :: String
                           , riDuration :: Int
                           } deriving (Read,Show)

data Options = Options { opStartPoint :: String
                       , opEndPoint :: String
                       , opTime :: Maybe String
                       , opDate :: Maybe String
                       , opCount :: Maybe Int
                       , showHelp :: Bool }

defaultOptions = Options { opStartPoint = ""
                         , opEndPoint = ""
                         , opTime = Nothing
                         , opDate = Nothing
                         , showHelp = False
                         , opCount = Nothing }

