{-# LANGUAGE Arrows #-}

module Scraping where

import Network.HTTP
import Network.URI
import System.IO
import System.Locale
import Data.Maybe
import Data.Time
import Data.Time.Clock (getCurrentTime)
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Text.Regex.Posix

import FinnTypes
import PageCache

fetchResults :: PageCache -> String -> IO (Either String FinnResult)
fetchResults cache url =
  do
    resp <- fetchUrlWithCaching cache uri
    case resp of
      Left x ->
        return $ Left (show x)
      Right page ->
        do
          items <- parseResultPage uri (cachedPageBody page)
          items' <- mapM (parseImages cache) items
          let result = FinnResult {resultUrl = url, resultItems = items'}
          return (Right result)
  where
    uri = fromJust $ parseURI url

parseImages :: PageCache -> FinnItem -> IO FinnItem
parseImages cache item =
  do
    putStrLn ("Fetching images in " ++ (show uri))
    resp <- fetchUrlWithCaching cache uri
    case resp of
      Left x ->
        return item
      Right page ->
        do
          images <- parseImagePage item uri (cachedPageBody page)
          return item {itemImages = images}
  where
    uri = fromJust $ parseURI ("http://www.finn.no/finn/torget/tilsalgs/viewimage?finnkode=" ++ (itemFinnKode item))

parseImagePage :: FinnItem -> URI -> String -> IO [FinnImage]
parseImagePage item uri body =
  do
    let doc = readString [withParseHTML yes, withWarnings no] body
    now <- getCurrentTime
    returnA $ runX $ (doc >>> css "span.thumbinnerwrap img" >>>
                      getAttrValue "src") >>. map (parseImageItem uri now)
  where
    parseImageItem :: URI -> UTCTime -> String -> FinnImage
    parseImageItem baseUrl time src =
      FinnImage{imageFinnKode = itemFinnKode item,
                imageCreatedAt = time,
                imageUpdatedAt = time,
                imageNormalSizeUrl = prefix ++ suffix,
                imageLargeSizeUrl = prefix ++ "_xl" ++ suffix}
      where
        [[_, prefix, suffix]] = src =~ "^(.*)_thumb(\\..*)" :: [[String]]

parseResultPage :: URI -> String -> IO [FinnItem]
parseResultPage uri body =
  do
    let doc = readString [withParseHTML yes, withWarnings no] body
    now <- getCurrentTime
    returnA $ runX $ (doc >>> css "div.fright.objectinfo") >>> (parseResultItem uri now)
  where
    parseResultItem :: ArrowXml a => URI -> UTCTime -> a XmlTree FinnItem
    parseResultItem baseUrl time =
      proc node -> do
        anchor <- css "div h2 a" >>> getAttrValue "href" -< node
        anchorText <- css "div h2 a" >>> (deep getText) -< node
        pubInfo <- css "dd[data-automation-id='dateinfo']" >>> (deep getText) -< node
        locationInfo <- css "dd[data-automation-id='location']" >>> (deep getText) -< node
        priceInfo <- css "div.strong.sharp" >>> (deep getText) -< node
        returnA -< FinnItem {
          itemFinnKode = extractFinnKode anchor,
          itemTitle = anchorText,
          itemUrl = qualifyUrl anchor,
          itemPublishedAt = extractDate pubInfo,
          itemCreatedAt = time,
          itemUpdatedAt = time,
          itemLocation = extractLocation locationInfo,
          itemPrice = extractPrice priceInfo,
          itemImages = []
        }
      where
        qualifyUrl url =
          show (nonStrictRelativeTo (fromJust $ parseRelativeReference url) baseUrl)

    extractFinnKode url = kode
      where [[_, kode]] = url =~ "finnkode=([^&]+)" :: [[String]]

    extractDate text = parsed
      where parsed = (readTime defaultTimeLocale "%d.%m.%Y %H:%M" text) :: UTCTime

    extractLocation :: String -> String
    extractLocation text = if splittable then base else text
      where
        splittable = sanitized =~ ", [0-9]+" :: Bool
        [[_, base]] = sanitized =~ "^(.*), [0-9]+.*" :: [[String]]

        -- FIXME: This is too naive
        sanitized = filter (\e -> e >= '\x0020' && e <= '\x007f') text

    extractPrice text = filter (\e -> e >= '0' && e <= '9') text
