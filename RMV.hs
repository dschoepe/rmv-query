{-# LANGUAGE RecordWildCards #-}
import Control.Applicative
import Data.Monoid
import Data.Maybe

import Network.RMV.Parse
import Network.RMV.Post
import Network.RMV.Output
import Network.RMV.Types

import Text.HTML.TagSoup
import System.Console.GetOpt
import System.Environment

options :: [OptDescr (Endo Options)]
options =
    [ Option ['h'] ["help"]
      (NoArg . Endo $ \o -> o { showHelp = True })
      "Show help"
    , Option ['f'] ["from"]
      (ReqArg (\s -> Endo $ \o -> o { opStartPoint = s }) "location")
      "Start point of the route"
    , Option ['t'] ["to"]
      (ReqArg (\s -> Endo $ \o -> o { opEndPoint = s }) "location")
      "End point of the route"
    , Option ['c'] ["count"]
      (ReqArg (\c -> Endo $ \o -> o { opCount = safeRead c }) "count")
      "Number of possible routes to be collected"
    , Option ['d'] ["date"]
      (ReqArg (\s -> Endo $ \o -> o { opDate = Just s }) "date")
      "Desired date"
    , Option ['w'] ["when","time"]
      (ReqArg (\s -> Endo $ \o -> o { opTime = Just s }) "time")
      "Desired time"
    ]
    where safeRead x = case reads x of
                         [(x,[])] -> Just x
                         _ -> Nothing

getRoutes opts = collectInfo . parseTags <$> sendRequest opts

getMultipleRoutes :: Options -> IO [[RouteInfo]]
getMultipleRoutes opts@(Options {..}) = do
  rs <- getRoutes opts
  let resultLength = length rs
      count = fromMaybe resultLength opCount
  case rs of
    [] -> return []
    _ | resultLength >= count -> return . take count $ rs
      | otherwise -> do 
            next <- getMultipleRoutes opts { opCount = subtract resultLength <$> opCount
                                          , opTime = Just . riEndTime . last . last $ rs }
            return . take count $ rs ++ next

parseOpts args =
    case getOpt Permute options args of
      (o,n,[]) -> return $ mconcat o `appEndo` defaultOptions
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo "USAGE:" options))

main = do
  opts <- parseOpts =<< getArgs
  if showHelp opts
     then putStrLn $ usageInfo "USAGE:" options
     else do
       routes <- getMultipleRoutes opts
       if null routes
          then putStrLn "Could not find any routes"
          else ppResult routes

testOpts = defaultOptions { opStartPoint = "kopernikusplatz"
                          , opEndPoint = "darmstadt, schloß"
                          , opCount = Just 3 }
