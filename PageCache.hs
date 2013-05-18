module PageCache where

import Control.Monad (when)
import Network.URI
import Network.HTTP
import Network.HTTP.Base
import Data.Maybe
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Text (pack, unpack)
import Data.Time
import Data.Time.Format
import Data.Time.RFC2822
import Data.Encoding
import Data.Encoding.ISO88591
import Data.Encoding.UTF8
import System.Locale
import Text.Regex.Posix
import Database.HDBC
import Database.HDBC.Sqlite3 hiding (Connection)
import qualified Database.HDBC.Sqlite3 as Sqlite3 (Connection)

data CachedPage = CachedPage {cachedPageUrl :: String,
                              cachedPageBody :: String,
                              cachedPageLastModifiedAt :: Maybe UTCTime}
  deriving (Eq, Show, Read)

data PageCache = PageCache {pageCacheConn :: IO Sqlite3.Connection}

data FetchedPage = FetchedPage {fetchedPageBody :: String,
                                fetchedPageLastModifiedAt :: Maybe UTCTime}

openPageCache :: FilePath -> IO PageCache
openPageCache path =
  do dbh <- connectSqlite3 path
     prepDB dbh
     return PageCache{pageCacheConn = return dbh}
  where
    prepDB :: IConnection conn => conn -> IO ()
    prepDB dbh =
      do tables <- getTables dbh
         when (not ("cached_pages" `elem` tables)) $
             do run dbh "create table cached_pages ( \
                         \url text primary key, \
                         \body text not null, \
                         \last_modified_at timestamp not null)" []
                return ()
         commit dbh

closePageCache :: PageCache -> IO ()
closePageCache cache =
  do dbh <- pageCacheConn cache
     commit dbh
     disconnect dbh

fetchUrlWithCaching :: PageCache -> URI -> IO (Either String CachedPage)
fetchUrlWithCaching cache uri =
  do
    cached <- getCachedPage cache url
    case cached of
      Nothing -> do
        -- TODO: Use If-Modified-Since to refresh
        body <- fetchUrl uri
        case body of
          Left x -> return $ Left ("Error connecting: " ++ show x)
          Right fetched -> do
            cached <- putCachedPage cache cached
            return $ Right cached
            where
              cached = CachedPage {cachedPageUrl = url,
                                   cachedPageBody = fetchedPageBody fetched,
                                   cachedPageLastModifiedAt = fetchedPageLastModifiedAt fetched}
      Just cached -> return $ Right cached
  where
    url = show uri

fetchUrl :: URI -> IO (Either String FetchedPage)
fetchUrl url =
  do resp <- fetchUrlRaw url
     case resp of
       Left x -> return $ Left x
       Right response -> do
         return $ Right fetched
         where
           contentType = contentTypeHeader response
           charset = charsetFromContentType contentType
           body = decodeFromCharset charset (rspBody response)
           fetched = FetchedPage {fetchedPageBody = body,
                                  fetchedPageLastModifiedAt = lastModifiedHeader response}

           lastModifiedHeader :: Response ByteString -> Maybe UTCTime
           lastModifiedHeader response =
             case (lookupHeader HdrLastModified (rspHeaders response)) of
               Just s ->
                 case (readRFC2822 s) of
                   Just z -> Just $ zonedTimeToUTC z
                   Nothing -> Nothing
               Nothing -> Nothing

           contentTypeHeader :: Response ByteString -> String
           contentTypeHeader response =
             case (lookupHeader HdrContentType (rspHeaders response)) of
               Just s -> s
               Nothing -> "application/octet-stream"

           charsetFromContentType :: String -> String
           charsetFromContentType contentType
             | hasCharset contentType = charset
             | otherwise = "utf-8"
             where
               [[_, charset]] = contentType =~ "charset=([^ ]+)"
               hasCharset s = s =~ "charset=([^ ]+)" :: Bool

           decodeFromCharset :: String -> ByteString -> String
           decodeFromCharset charset bytes = Data.Text.unpack (reencode bytes)
             -- FIXME: Make work for other than iso-8859-1
             where
               reencode = Data.Text.pack . Data.ByteString.Char8.unpack

fetchUrlRaw :: URI -> IO (Either String (Response ByteString))
fetchUrlRaw url =
  do resp <- simpleHTTP request
     case resp of
       -- TODO: Return real error type
       Left x -> return $ Left ("Error connecting: " ++ show x)
       Right r ->
         case rspCode r of
           (2, _, _) ->
             return $ Right r
           (3, _, _) ->
             case findHeader HdrLocation r of
               Nothing -> return $ Left (show r)
               Just url ->
                 let uri = fromJust $ parseURI url
                 in fetchUrlRaw uri
           _ -> return $ Left (show r)
  where request = Request {rqURI = url,
                           rqMethod = GET,
                           rqHeaders = [],
                           rqBody = (empty :: ByteString)}

getCachedPage :: PageCache -> String -> IO (Maybe CachedPage)
getCachedPage cache url =
  do
    dbh <- pageCacheConn cache
    res <- quickQuery' dbh
           "select url, body, last_modified_at from cached_pages where url = ? limit 1"
           [toSql url]
    case res of
      [[url, body, lastModifiedAt]] -> do
        putStrLn ("Cache hit " ++ fromSql url)
        return (Just CachedPage {
          cachedPageUrl = fromSql url,
          cachedPageBody = fromSql body,
          cachedPageLastModifiedAt = fromSql lastModifiedAt
        })
      [] -> do
        putStrLn ("Cache miss " ++ url)
        return Nothing
      x -> fail $ "Unexpected rows"

putCachedPage :: PageCache -> CachedPage -> IO CachedPage
putCachedPage cache page =
  do
    dbh <- pageCacheConn cache
    run dbh "insert or ignore into cached_pages (\
      \url, body, last_modified_at) values (?, ?, ?)" (map toSql [
      cachedPageUrl page,
      cachedPageBody page,
      timeAsString (cachedPageLastModifiedAt page)])
    commit dbh
    return page
  where
    timeAsString t =
      case t of
        Just t' -> formatTime defaultTimeLocale "%s" t'
        Nothing -> ""
