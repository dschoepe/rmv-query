module Network.RMV.Types
    (RouteInfo(..),Options(..),defaultOptions)
        where

data RouteInfo = RouteInfo { riStartTime :: Maybe String
                           , riEndTime :: Maybe String
                           , riStartPoint :: String
                           , riLine :: String
                           , riEndPoint :: String
                           , riDuration :: Maybe Int
                           } deriving (Read,Show)

data Options = Options { opStartPoint :: String
                       , opEndPoint :: String
                       , opTime :: Maybe String
                       , opDate :: Maybe String
                       , opCount :: Maybe Int
                       , opConfigFile :: Maybe String
                       , aliases :: [(String,String)]
                       , opTemplate :: String
                       , showHelp :: Bool }

defaultOptions = Options { opStartPoint = ""
                         , opEndPoint = ""
                         , opTime = Nothing
                         , opDate = Nothing
                         , showHelp = False
                         , aliases = []
                         , opTemplate = "$duration | $start_time $start -< $line >- $end $end_time"
                         , opConfigFile = Nothing
                         , opCount = Nothing }

