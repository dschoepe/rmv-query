{-# LANGUAGE NamedFieldPuns, RecordWildCards, ViewPatterns #-}
module Network.RMV.Parse
    (collectInfo) where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match

import Network.RMV.Types

tables :: [Tag] -> [[Tag]]
tables = partitions $ tagOpen (=="a") (anyAttr (\(n,v) -> n == "name" && "cis_C0-" `isPrefixOf` v))

routes :: [Tag] -> [([Tag],[Tag])]
routes = pairList . init . partitions (~== "<tr class=\"tpDetails \">")
    where pairList (x:y:xs) = (x,y) : pairList xs
          pairList _ = []

firstText :: [Tag] -> Maybe String
firstText = fmap fromTagText . listToMaybe . filter isTagText

titleP :: (String -> Bool) -> [Attribute] -> Bool
titleP p = anyAttr $ \(n,x) -> n == "title" && p x

parseRoute :: Int -> ([Tag],[Tag]) -> Maybe RouteInfo
parseRoute num (from,to) = do
  let name = firstText <=< listToMaybe . sections isNameTag
      isNameTag = tagOpen (=="a") (titleP ("Haltestelleninformation:" `isPrefixOf`))
      time x = fmap (filter (/='\n')) . firstText <=< listToMaybe . sections (~==x)
  line <- firstText =<< listToMaybe (sections (~=="<a title=Fahrtinformation>") from)
  depTime <- time ("<td headers=\"hafasDTL"++show num ++"_TimeDep\">") from
  arrTime <- time ("<td headers=\"hafasDTL"++show num ++"_TimeArr\">") to
  start <- name from
  end <- name to
  return RouteInfo { riStartPoint = start
                   , riEndPoint = end
                   , riStartTime = depTime
                   , riEndTime = arrTime
                   , riDuration = arrTime ^-^ depTime
                   , riLine = line}

(^-^) :: String -> String -> Int
t1 ^-^ t2 = (h1 - h2) * 60 + (m1 - m2)
    where (h1,m1) = break' t1
          (h2,m2) = break' t2
          break' = (read *** read) . second tail . break (==':')

-- | Returns all routes found in a given list of Tags
collectInfo :: [Tag] -> [[RouteInfo]]
collectInfo = map (\(n,x) -> catMaybes . map (parseRoute n) . routes $ x) . zip [0..] . tables
