{-# LANGUAGE RecordWildCards #-}
import Control.Arrow
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
import System.Directory

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
    , Option ['T','w'] ["when","time"]
      (ReqArg (\s -> Endo $ \o -> o { opTime = Just s }) "time")
      "Desired time"
    , Option ['C'] ["config"]
      (ReqArg (\s -> Endo $ \o -> o { opConfigFile = Just s}) "config")
      "Path to configuration file(default is ~/.rmv-query)"
    , Option ['a'] ["aliases"]
      (ReqArg (\s -> Endo $ \o -> o { aliases = concatMap fst (reads s) ++ aliases o }) "aliases")
      ("Aliases for destinations or starting points. Expected in this form:\n"++
       "[(\"alias\",\"normal name\"),(\"other alias\",\"other name\")]\n" ++
       "This option is intended mainly for the use in your configuration and can occur multiple times.")
    , Option [] ["template"]
      (ReqArg (\s -> Endo $ \o -> o { opTemplate = s }) "template")
      ("Template to use for displaying a route. Available variables are:\n"++
       "$duration - duration of the route\n"++
       "$start_time - time of departure at stop\n"++
       "$end_time - time of arrival at next stop\n"++
       "$start - Name of starting point(e.g. Frankfurt)\n"++
       "$end - Name of end point(e.g. Darmstadt)"
      )
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

parseConfig :: FilePath -> IO (Maybe (Endo Options))
parseConfig file = do
  exists <- doesFileExist file
  if exists
     then do
       let pairToList (x,y) = [x,y]
       args <- concatMap (pairToList . second tail . break (==' ')) . lines <$> readFile file
       case getOpt Permute options args of
         (o,n,[]) -> return . Just . mconcat $ o
         (_,_,errs) -> do
              putStrLn $ "Errors in configuration file\n" ++ concat errs
              return Nothing
     else putStrLn "Specified configuration file does not exist, using default options" >>
          return Nothing

myUsageInfo = usageInfo "USAGE: rmv-query [OPTIONS]" options

parseOpts args =
    case getOpt Permute options args of
      (o,n,[]) -> return $ mconcat o
      (_,_,errs) -> ioError (userError (concat errs ++ myUsageInfo))

main = do
  optEndo <- parseOpts =<< getArgs
  let opts = optEndo `appEndo` defaultOptions
  if showHelp opts
     then putStrLn myUsageInfo
     else do
       defaultConfig <- (++"/.rmv-query") <$> getHomeDirectory
       configEndo <- parseConfig . fromMaybe defaultConfig . opConfigFile $ opts
       let opts' = maybe mempty (optEndo `mappend`) configEndo `appEndo` opts
       routes <- getMultipleRoutes opts'
       if null routes
          then putStrLn "Could not find any routes"
          else ppResult opts' routes
