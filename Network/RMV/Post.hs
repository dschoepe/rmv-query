{-# LANGUAGE RecordWildCards #-}
module Network.RMV.Post
    (sendRequest) where

import Network.RMV.Types
import Network.RMV.Output

import Data.Char
import Data.List
import Data.Maybe
import Network.Browser
import Network.HTTP
import Network.URI
import System.Time

rmvURI = URI { uriScheme = "http:"
             , uriAuthority = Just URIAuth { uriUserInfo = ""
                                           , uriRegName = "www.rmv.de"
                                           , uriPort = ":80" }
             , uriPath = "/auskunft/bin/jp/query.exe/dn"
             , uriQuery = "?L=vs_rmv&"
             , uriFragment = "" }

reqBody :: Options -> IO String
reqBody opts = do
  time <- getClockTime >>= toCalendarTime
  let curDate = intercalate "." . map (pad . show . ($time)) $
                [ ctDay, (+1) . fromEnum . ctMonth, ctYear]
      curTime = ppTime (ctHour time * 60 + ctMin time)
      pad xs = replicate (2 - length xs) '0' ++ xs
  return $ intercalate "&"
             [ "queryPageDisplayed=yes"
             , "REQ0JourneyStopsN=-1"
             , "REQ0HafasNumCons1=1"
             , "REQ0HafasNumCons0=4"
             , "REQ0JourneyStopsS0A=255"
             , "REQ0JourneyStopsS0G=" ++ urlEncode (opStartPoint opts)
             , "REQ0JourneyStopsS0ID="
             , "REQ0JourneyStopsZ0A=255"
             , "REQ0JourneyStopsZ0G=" ++ urlEncode (opEndPoint opts)
             , "REQ0JourneyStopsZ0ID="
             , "existUnsharpSearch=yes"
             , "iER=no"
             , "REQ0HafasOptimize1=1"
             , "REQ0JourneyDate=" ++ urlEncode (fromMaybe curDate $ opDate opts)
             , "wDayExt0=Mo%7CDi%7CMi%7CDo%7CFr%7CSa%7CSo"
             , "REQ0JourneyTime=" ++ urlEncode (fromMaybe curTime $ opTime opts)
             , "REQ0HafasSearchForw=1"
             , "REQ0JourneyProductMask=1%3A11111111111111111"
             , "start=Verbindungen+suchen"
             ]

userAgentMimicIE = "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0; .NET CLR 1.1.4322; .NET CLR 2.0.50727; InfoPath.1)"

expandAliases :: Options -> Options
expandAliases opts@(Options{..}) =
    opts { opStartPoint = expand opStartPoint
         , opEndPoint = expand opEndPoint }
    where expand s = maybe s snd $ find ((~=s) . fst) aliases
          a ~= b = map toLower a == map toLower b

-- | Sends a request with the specified options
sendRequest :: Options -> IO String
sendRequest opts = do
  req <- reqBody $ expandAliases opts
  rsp <- Network.Browser.browse $ do
          setAllowRedirects True
          setOutHandler $ const (return ())
          request Request { rqURI = rmvURI
                          , rqMethod = POST
                          , rqHeaders = [ Header HdrContentLength $ show (length req)
                                        , Header HdrUserAgent userAgentMimicIE ]
                          , rqBody = req
                          }
  return . rspBody . snd $ rsp

testOpts = defaultOptions { opStartPoint = "kopernikusplatz"
                          , opEndPoint = "darmstadt, schloß"
                          , opCount = Just 3 }
