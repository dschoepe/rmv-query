{-# LANGUAGE NamedFieldPuns, RecordWildCards, ViewPatterns #-}
module Network.RMV.Parse
    (collectInfo,safeRead) where

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

firstSectionText :: (Tag -> Bool) -> [Tag] -> Maybe String
firstSectionText pred = firstText <=< listToMaybe . sections pred

safeRead :: Read a => String -> Maybe a
safeRead x = case reads x of
               [(x,[])] -> Just x
               _ -> Nothing

titleP :: (String -> Bool) -> [Attribute] -> Bool
titleP p = anyAttr $ \(n,x) -> n == "title" && p x

line :: Int -> [Tag] -> Maybe String
line num = altText <=< getImgTag <=< findSection
    where altText = find (/="") . pure . fromAttrib "alt"
          getImgTag = nth' 1 . filter isTagOpen
          findSection = listToMaybe . sections (~==lineInfo)
          lineInfo = "<td headers=\"hafasDTL"++show num++"_Products\">"
          nth' n lst | length lst > n = Just $ lst !! n
                     | otherwise = Nothing

parseRoute :: Int -> ([Tag],[Tag]) -> Maybe RouteInfo
parseRoute num (from,to) = do
  let name = firstSectionText isNameTag
      isNameTag = tagOpen (=="a") (titleP ("Haltestelleninformation:" `isPrefixOf`))
      time x = fmap (filter (/='\n')) . firstSectionText (~==x)
  line' <- line num to `mplus` line num from
  let depTime = time ("<td headers=\"hafasDTL"++show num ++"_TimeDep\">") from
      arrTime = time ("<td headers=\"hafasDTL"++show num ++"_TimeArr\">") to
  start <- name from
  end <- name to
  return RouteInfo { riStartPoint = start
                   , riEndPoint = end
                   , riStartTime = depTime
                   , riEndTime = arrTime
                   , riDuration = join $ (^-^) <$> arrTime <*> depTime
                   , riLine = line'
                   }

(^-^) :: String -> String -> Maybe Int
t1 ^-^ t2 = do
  let break' = fromPair . (safeRead *** safeRead) . second tail . break (==':')
      fromPair (Just x,Just y) = Just (x,y)
      fromPair _ = Nothing
  (h1,m1) <- break' t1
  (h2,m2) <- break' t2
  return $ (h1 - h2) * 60 + (m1 - m2)

-- | Returns all routes found in a given list of Tags
collectInfo :: [Tag] -> [[RouteInfo]]
collectInfo = map (\(n,x) -> catMaybes . map (parseRoute n) . routes $ x) . zip [0..] . tables
