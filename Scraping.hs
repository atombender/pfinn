{-# LANGUAGE OverloadedStrings, Arrows #-}

module Scraping where

import Control.Monad.IO.Class
import Network.HTTP
import Network.URI
import System.IO
import System.Locale
import Data.Maybe
import Data.List (isInfixOf, nubBy)
import Data.Time
import Data.Time.Clock (getCurrentTime)
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Text.Regex.Posix

import FinnTypes
import PageCache

-- Fetch a search result page and scrape it, returning result data.
scrapeResults :: PageCache -> String -> IO (Either String FinnResult)
scrapeResults cache url =
  do
    resp <- fetchUrlWithCaching cache uri (Just 300)
    case resp of
      Left x ->
        return $ Left (show x)
      Right page ->
        do
          items <- scrapeResultPage uri (cachedPageBody page)
          items' <- mapM (scrapeItem cache) items
          let result = FinnResult {resultUrl = url, resultItems = catMaybes items'}
          return (Right result)
  where
    uri = fromJust $ parseURI url

    -- Scrape items from a result page.
    scrapeResultPage :: URI -> String -> IO [FinnItem]
    scrapeResultPage uri body =
      do
        let doc = readString [withParseHTML yes, withWarnings no] body
        now <- getCurrentTime
        items <- liftIO $ runX $ (doc >>> css "div.fright.objectinfo")
                                 >>> (parseResultDiv uri now)
        return $ nubBy (\a b -> itemFinnKode a == itemFinnKode b) items
      where
        parseResultDiv :: ArrowXml a => URI -> UTCTime -> a XmlTree FinnItem
        parseResultDiv baseUrl time =
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
              itemAddress = "",
              itemSellerName = "",
              itemImages = []
            }
          where
            qualifyUrl url =
              show (nonStrictRelativeTo (fromJust $ parseRelativeReference url) baseUrl)

        extractFinnKode url = kode
          where [[_, kode]] = url =~ ("finnkode=([^&]+)" :: String) :: [[String]]

        extractDate text = parsed
          where parsed = (readTime defaultTimeLocale "%d.%m.%Y %H:%M" text) :: UTCTime

        extractLocation :: String -> String
        extractLocation text = if splittable then base else text
          where
            splittable = sanitized =~ (", [0-9]+" :: String) :: Bool
            [[_, base]] = sanitized =~ ("^(.*), [0-9]+.*" :: String) :: [[String]]

            -- FIXME: This is too naive
            sanitized = filter (\e -> e >= '\x0020' && e <= '\x007f') text

        extractPrice text = filter (\e -> e >= '0' && e <= '9') text

-- Scrape the item's main page, augmenting the item with more data.
scrapeItem :: PageCache -> FinnItem -> IO (Maybe FinnItem)
scrapeItem cache item =
  do
    resp <- fetchUrlWithCaching cache uri Nothing
    case resp of
      Left x -> return $ Just item
      Right page -> parseItemPage item uri (cachedPageBody page)
  where
    uri = fromJust $ parseURI ("http://www.finn.no/finn/torget/tilsalgs/annonse?finnkode=" ++ (itemFinnKode item))

    parseItemPage :: FinnItem -> URI -> String -> IO (Maybe FinnItem)
    parseItemPage item uri body
      | isDeleted body = return $ Just item
      | otherwise = do
          let doc = readString [withParseHTML yes, withWarnings no] body
          now <- getCurrentTime
          items <- runX $ doc >>> (parsePage now item)
          case items of
            [item'] -> return $ Just item'
            otherwise -> return Nothing
      where
        isDeleted body = "Annonsen er slettet" `isInfixOf` body

        parsePage :: (ArrowXml a) => UTCTime -> FinnItem -> a XmlTree FinnItem
        parsePage now node =
          do
            proc node -> do
              imageUrls <- listA (css "#thumbnails .thumbinnerwrap img"
                                  >>> getAttrValue "src") -< node
              addressInfo <- (css "h3 figcaption a"
                              >>> (deep getText)) -< node

              sellerInfo <- (css ".object #inlineEmail h2" >>> (deep getText))
                            `orElse`
                            (css "h2#partnername" >>> (deep getText)) -< node
              returnA -< item {
                itemImages = map (parseImage now) imageUrls,
                itemAddress = addressInfo,
                itemSellerName = sellerInfo
              }

        parseImage :: UTCTime -> String -> FinnImage
        parseImage time src =
          FinnImage{imageFinnKode = itemFinnKode item,
                    imageCreatedAt = time,
                    imageUpdatedAt = time,
                    imageNormalSizeUrl = prefix ++ suffix,
                    imageLargeSizeUrl = prefix ++ "_xl" ++ suffix}
          where
            [[_, prefix, suffix]] = src =~ ("^(.*)_thumb(\\..*)" :: String) :: [[String]]
