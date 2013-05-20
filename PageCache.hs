module PageCache where

import Control.Monad (when)
import Network.URI
import Network.HTTP
import Data.Maybe
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Text (pack, unpack)
import Data.Time
import Data.Time.RFC2822
import System.Locale
import Text.Regex.Posix
import Database.HDBC
import Database.HDBC.PostgreSQL hiding (Connection)
import qualified Database.HDBC.PostgreSQL as PostgreSQL (Connection)

data CachedPage = CachedPage {cachedPageUrl :: String,
                              cachedPageBody :: String,
                              cachedPageLastModifiedAt :: Maybe UTCTime}
  deriving (Eq, Show, Read)

data PageCache = PageCache {pageCacheConn :: IO PostgreSQL.Connection}

data FetchedPage = FetchedPage {fetchedPageBody :: String,
                                fetchedPageLastModifiedAt :: Maybe UTCTime}

openPageCache :: IO PageCache
openPageCache =
  do
    params <- readFile "database.conf"
    db <- connectPostgreSQL params
    setupSchema db
    return PageCache{pageCacheConn = return db}
  where
    setupSchema :: IConnection conn => conn -> IO ()
    setupSchema db =
      do tables <- getTables db
         when (not ("cached_pages" `elem` tables)) $
           do
             _ <- run db "create table cached_pages ( \
                         \url text primary key, \
                         \body text not null, \
                         \last_modified_at timestamp)" []
             return ()
         commit db

closePageCache :: PageCache -> IO ()
closePageCache cache =
  do db <- pageCacheConn cache
     commit db
     disconnect db

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
            page <- putCachedPage cache cachedPage
            return $ Right page
            where
              cachedPage = CachedPage {cachedPageUrl = url,
                                   cachedPageBody = fetchedPageBody fetched,
                                   cachedPageLastModifiedAt = fetchedPageLastModifiedAt fetched}
      Just c -> return $ Right c
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

  where
    lastModifiedHeader :: Response ByteString -> Maybe UTCTime
    lastModifiedHeader rs =
      case (lookupHeader HdrLastModified (rspHeaders rs)) of
        Just s ->
          case (readRFC2822 s) of
            Just z -> Just $ zonedTimeToUTC z
            Nothing -> Nothing
        Nothing -> Nothing

    contentTypeHeader :: Response ByteString -> String
    contentTypeHeader rs =
      case (lookupHeader HdrContentType (rspHeaders rs)) of
        Just s -> s
        Nothing -> "application/octet-stream"

    charsetFromContentType :: String -> String
    charsetFromContentType ctype
      | hasCharset ctype = cset
      | otherwise = "utf-8"
      where
        [[_, cset]] = ctype =~ "charset=([^ ]+)"
        hasCharset s = s =~ "charset=([^ ]+)" :: Bool

    decodeFromCharset :: String -> ByteString -> String
    decodeFromCharset _ bytes = Data.Text.unpack (reencode bytes)
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
                   Just u -> fetchUrlRaw (fromJust $ parseURI u)
               _ -> return $ Left (show r)
      where request = Request {rqURI = url,
                               rqMethod = GET,
                               rqHeaders = [],
                               rqBody = (empty :: ByteString)}

getCachedPage :: PageCache -> String -> IO (Maybe CachedPage)
getCachedPage cache url =
  do
    db <- pageCacheConn cache
    res <- quickQuery' db
           "select body, last_modified_at from cached_pages where url = ? limit 1"
           [toSql url]
    case res of
      [[body, lastModifiedAt]] -> do
        putStrLn ("Cache hit " ++ url)
        return (Just CachedPage {
          cachedPageUrl = url,
          cachedPageBody = fromSql body,
          cachedPageLastModifiedAt = fromSql lastModifiedAt
        })
      [] -> do
        putStrLn ("Cache miss " ++ url)
        return Nothing
      _ -> fail $ "Unexpected rows"

putCachedPage :: PageCache -> CachedPage -> IO CachedPage
putCachedPage cache page =
  do
    db <- pageCacheConn cache
    _ <- run db "insert into cached_pages (\
                \url, body, last_modified_at) values (?, ?, ?)"
         [toSql $ cachedPageUrl page,
         toSql $ cachedPageBody page,
         maybeFormatPgTimestamp (cachedPageLastModifiedAt page)]
    commit db
    return page

parsePgTimestamp :: String -> UTCTime
parsePgTimestamp s = readTime defaultTimeLocale "%s" s :: UTCTime

formatPgTimestamp :: UTCTime -> String
formatPgTimestamp t = formatTime defaultTimeLocale "%F %T" t

maybeFormatPgTimestamp :: Maybe UTCTime -> SqlValue
maybeFormatPgTimestamp t =
  case t of
    Just t' -> toSql (formatTime defaultTimeLocale "%s" t')
    Nothing -> SqlNull
