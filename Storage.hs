module Storage where

import Control.Monad (when)
import Network.URI
import Network.HTTP
import Network.HTTP.Base
import System.Locale
import Data.Time
import Data.Time.Clock (getCurrentTime)
import Data.Maybe
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Text (pack, unpack)
import Data.Encoding
import Data.Encoding.ISO88591
import Data.Encoding.UTF8
import Text.Regex.Posix
import Database.HDBC
import Database.HDBC.Sqlite3 hiding (Connection)
import qualified Database.HDBC.Sqlite3 as Sqlite3 (Connection)

import FinnTypes

data Store = Store {storeConn :: IO Sqlite3.Connection}

data FindOptions = FindOptions {findOptionsOffset :: Integer,
                                findOptionsLimit :: Integer,
                                findOptionsBeforeId :: Maybe Integer}

openStore :: FilePath -> IO Store
openStore path =
  do
    db <- connectSqlite3 path
    setupSchema db
    return Store{storeConn = return db}
  where
    setupSchema :: IConnection conn => conn -> IO ()
    setupSchema db =
      do
        tables <- getTables db
        when (not ("items" `elem` tables)) $
          do run db "create table items (\
                     \finn_kode integer primary key, \
                     \title text not null, \
                     \url text not null, \
                     \published_at timestamp not null, \
                     \created_at timestamp not null, \
                     \updated_at timestamp not null, \
                     \location text, \
                     \price text)" []
             return ()
        when (not ("images" `elem` tables)) $
          do run db "create table images (\
                     \finn_kode integer not null references items (finn_kode), \
                     \created_at timestamp not null, \
                     \updated_at timestamp not null, \
                     \large_url text not null, \
                     \normal_url text not null)" []
             return ()
        when (not ("read_states" `elem` tables)) $
          do run db "create table read_states (\
                     \finn_kode integer not null references items (finn_kode), \
                     \read_at timestamp not null)" []
             return ()
        commit db

closeStore :: Store -> IO ()
closeStore store =
  do
    db <- storeConn store
    commit db
    disconnect db

isRead :: Store -> FinnKode -> IO Bool
isRead store finnKode =
  do
    db <- storeConn store
    res <- quickQuery' db
           ("select 1 from read_states where finn_kode = ? limit 1")
           (map toSql [finnKode])
    case res of
      [row] -> return True
      _ -> return False

markRead :: Store -> FinnKode -> IO ()
markRead store finnKode =
  do
    db <- storeConn store
    now <- getCurrentTime
    run db "insert or ignore into read_states (\
      \finn_kode, read_at) values (?, ?)" (map toSql [
      finnKode,
      formatTime defaultTimeLocale "%s" now])
    return ()

findItems :: Store -> FindOptions -> IO [FinnItem]
findItems store options =
  do
    db <- storeConn store
    case (findOptionsBeforeId options) of
      Just id -> do
        res <- quickQuery' db
               "select finn_kode from items where finn_kode < ? \
               \and finn_kode not in (select finn_kode from read_states) \
               \order by finn_kode desc \
               \limit ? offset ?" (map toSql [
               id,
               findOptionsLimit options,
               findOptionsOffset options])
        items <- mapM (getItemByFinnKode store) (map (\[e] -> fromSql e) res)
        return $ (catMaybes items)
      Nothing -> do
        res <- quickQuery' db
               "select finn_kode from items \
               \where finn_kode not in (select finn_kode from read_states) \
               \order by finn_kode desc \
               \limit ? offset ?" (map toSql [
               findOptionsLimit options,
               findOptionsOffset options])
        items <- mapM (getItemByFinnKode store) (map (\[e] -> fromSql e) res)
        return $ (catMaybes items)

getItemByFinnKode :: Store -> FinnKode -> IO (Maybe FinnItem)
getItemByFinnKode store finnKode =
  do
    db <- storeConn store
    res <- quickQuery' db
           "select finn_kode, title, url, published_at, created_at, \
           \updated_at, location, price from items where finn_kode = ? limit 1"
           [toSql finnKode]
    case res of
      [row] -> do
        let item = itemFromRow row
        images <- getImages
        let item' = item {itemImages = images}
        return $ Just item'
      [] -> do
        putStrLn ("Store miss " ++ finnKode)
        return Nothing
      x -> fail $ "Unexpected rows"
  where
    getImages =
      do
        db <- storeConn store
        res <- quickQuery' db
               "select finn_kode, created_at, updated_at, normal_url, large_url \
               \from images where finn_kode = ?"
               [toSql finnKode]
        return $ (map imageFromRow res)

saveItem :: Store -> FinnItem -> IO ()
saveItem store item =
  do
    maybeOldItem <- getItemByFinnKode store (itemFinnKode item)
    case maybeOldItem of
      Nothing -> do
        createItem store item
        mapM_ (\image -> saveItemImage store item image) $ itemImages item
      Just old -> do
        updateItem store (mergeItem old item)
  where
    createItem :: Store -> FinnItem -> IO ()
    createItem store item =
      do
        putStrLn ("Creating item finnkode " ++ (itemFinnKode item))
        db <- storeConn store
        run db "insert into items (\
          \title, url, finn_kode, published_at, created_at, updated_at, \
          \location, price) values (?, ?, ?, ?, ?, ?, ?, ?)"
          (map toSql [
            itemTitle item,
            itemUrl item,
            itemFinnKode item,
            formatTime defaultTimeLocale "%s" (itemPublishedAt item),
            formatTime defaultTimeLocale "%s" (itemCreatedAt item),
            formatTime defaultTimeLocale "%s" (itemUpdatedAt item),
            itemLocation item,
            itemPrice item])
        return ()

    saveItemImage :: Store -> FinnItem -> FinnImage -> IO ()
    saveItemImage store item image =
      do
        maybeOldImage <- getMatchingImage store item image
        case maybeOldImage of
          Nothing -> do
            createItemImage store item image
      where
        getMatchingImage :: Store -> FinnItem -> FinnImage -> IO (Maybe FinnImage)
        getMatchingImage store item image =
          do
            db <- storeConn store
            res <- quickQuery' db
                   "select finn_kode, created_at, updated_at, \
                   \normal_url, large_url from images where finn_kode = ? \
                   \and normal_url = ? and large_url = ? limit 1" (map toSql [
                   itemFinnKode item,
                   imageNormalSizeUrl image,
                   imageLargeSizeUrl image])
            case res of
              [row] -> return $ Just (imageFromRow row)
              [] -> return Nothing

        createItemImage :: Store -> FinnItem -> FinnImage -> IO ()
        createItemImage store item image =
          do
            putStrLn "Creating an image"
            db <- storeConn store
            run db "insert into images (\
              \finn_kode, created_at, updated_at, normal_url, large_url) \
              \values (?, ?, ?, ?, ?)"
              (map toSql [
                itemFinnKode item,
                formatTime defaultTimeLocale "%s" (imageCreatedAt image),
                formatTime defaultTimeLocale "%s" (imageUpdatedAt image),
                imageNormalSizeUrl image,
                imageLargeSizeUrl image])
            return ()

    updateItem :: Store -> FinnItem -> IO ()
    updateItem store item =
      do
        db <- storeConn store
        run db "update items set title = ?, url = ?, published_at = ?, \
          \created_at = ?, updated_at = ?, location = ?, price = ? \
          \where finn_kode = ?"
          (map toSql [
            itemTitle item,
            itemUrl item,
            formatTime defaultTimeLocale "%s" (itemPublishedAt item),
            formatTime defaultTimeLocale "%s" (itemCreatedAt item),
            formatTime defaultTimeLocale "%s" (itemUpdatedAt item),
            itemLocation item,
            itemPrice item,
            itemFinnKode item])
        return ()

    mergeItem :: FinnItem -> FinnItem -> FinnItem
    mergeItem old new = new

itemFromRow :: [SqlValue] -> FinnItem
itemFromRow [finnKode, title, url, publishedAt, createdAt, updatedAt, location, price] =
  FinnItem {
    itemFinnKode = fromSql finnKode,
    itemTitle = fromSql title,
    itemUrl = fromSql url,
    itemPublishedAt = parseTimestamp $ fromSql publishedAt,
    itemCreatedAt = parseTimestamp $ fromSql createdAt,
    itemUpdatedAt = parseTimestamp $ fromSql updatedAt,
    itemLocation = fromSql location,
    itemPrice = fromSql price,
    itemImages = []
  }

imageFromRow :: [SqlValue] -> FinnImage
imageFromRow [finnKode, createdAt, updatedAt, normalUrl, largeUrl] =
  FinnImage {
    imageFinnKode = fromSql finnKode,
    imageCreatedAt = parseTimestamp $ fromSql createdAt,
    imageUpdatedAt = parseTimestamp $ fromSql updatedAt,
    imageNormalSizeUrl = fromSql normalUrl,
    imageLargeSizeUrl = fromSql largeUrl
  }

parseTimestamp :: String -> UTCTime
parseTimestamp s = readTime defaultTimeLocale "%s" s :: UTCTime
