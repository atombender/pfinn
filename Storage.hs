{-# LANGUAGE OverloadedStrings #-}

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
import Database.HDBC.PostgreSQL hiding (Connection)
import qualified Database.HDBC.PostgreSQL as PostgreSQL (Connection)

import FinnTypes

data Store = Store {storeConn :: IO PostgreSQL.Connection}

data FindOptions = FindOptions {findOptionsOffset :: Integer,
                                findOptionsLimit :: Integer,
                                findOptionsBeforeId :: Maybe Integer}

openDefaultStore :: IO Store
openDefaultStore =
  do
    params <- readFile "database.conf"
    store <- openStore params
    return store

openStore :: String -> IO Store
openStore connectionParams =
  do
    db <- connectPostgreSQL connectionParams
    setupSchema db
    return Store {storeConn = return db}
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
                     \address text, \
                     \seller_name text, \
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
                     \finn_kode integer primary key references items (finn_kode), \
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
    res <- quickQuery' db
           "select read_at from read_states where finn_kode = ? limit 1"
           [toSql finnKode]
    case res of
      [row] -> do
        run db "update read_states set read_at = ? \
          \where finn_kode = ?" [
          toSql $ formatPgTimestamp now,
          toSql finnKode]
        return ()
      [] -> do
        run db "insert into read_states (\
          \finn_kode, read_at) values (?, ?)" [
          toSql finnKode,
          toSql $ formatPgTimestamp now]
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
           \updated_at, location, address, seller_name, price from items where finn_kode = ? limit 1"
           [toSql finnKode]
    case res of
      [row] -> do
        let item = itemFromRow row
        images <- getImages
        let item' = item {itemImages = images}
        return $ Just item'
      [] -> do
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

itemExists :: Store -> FinnKode -> IO Bool
itemExists store finnKode =
  do
    mItem <- getItemByFinnKode store finnKode
    case mItem of
      Nothing -> return False
      Just _ -> return True

saveItem :: Store -> FinnItem -> IO Bool
saveItem store item =
  do
    mOldItem <- getItemByFinnKode store (itemFinnKode item)
    case mOldItem of
      Nothing -> do
        createItem store item
        mapM_ (\image -> saveItemImage store item image) $ itemImages item
        return True
      Just old -> do
        updateItem store (mergeItem old item)
        return False
  where
    createItem :: Store -> FinnItem -> IO ()
    createItem store item =
      do
        db <- storeConn store
        run db "insert into items (\
          \title, url, finn_kode, published_at, created_at, updated_at, \
          \location, address, seller_name, price) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
          (map toSql [
            itemTitle item,
            itemUrl item,
            itemFinnKode item,
            formatPgTimestamp (itemPublishedAt item),
            formatPgTimestamp (itemCreatedAt item),
            formatPgTimestamp (itemUpdatedAt item),
            itemLocation item,
            itemAddress item,
            itemSellerName item,
            itemPrice item])
        return ()

    saveItemImage :: Store -> FinnItem -> FinnImage -> IO ()
    saveItemImage store item image =
      do
        mImage <- getMatchingImage store item image
        case mImage of
          Nothing -> do
            createItemImage store item image
          _ -> do return ()
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
            db <- storeConn store
            run db "insert into images (\
              \finn_kode, created_at, updated_at, normal_url, large_url) \
              \values (?, ?, ?, ?, ?)"
              (map toSql [
                itemFinnKode item,
                formatPgTimestamp (imageCreatedAt image),
                formatPgTimestamp (imageUpdatedAt image),
                imageNormalSizeUrl image,
                imageLargeSizeUrl image])
            return ()

    updateItem :: Store -> FinnItem -> IO ()
    updateItem store item =
      do
        db <- storeConn store
        run db "update items set title = ?, url = ?, published_at = ?, \
          \created_at = ?, updated_at = ?, location = ?, address = ?, seller_name = ?, \
          \price = ? where finn_kode = ?"
          (map toSql [
            itemTitle item,
            itemUrl item,
            formatPgTimestamp (itemPublishedAt item),
            formatPgTimestamp (itemCreatedAt item),
            formatPgTimestamp (itemUpdatedAt item),
            itemLocation item,
            itemAddress item,
            itemSellerName item,
            itemPrice item,
            itemFinnKode item])
        return ()

    mergeItem :: FinnItem -> FinnItem -> FinnItem
    mergeItem old new = new

itemFromRow :: [SqlValue] -> FinnItem
itemFromRow [finnKode, title, url, publishedAt, createdAt, updatedAt, location, address, sellerName, price] =
  FinnItem {
    itemFinnKode = fromSql finnKode,
    itemTitle = fromSql title,
    itemUrl = fromSql url,
    itemPublishedAt = parsePgTimestamp $ fromSql publishedAt,
    itemCreatedAt = parsePgTimestamp $ fromSql createdAt,
    itemUpdatedAt = parsePgTimestamp $ fromSql updatedAt,
    itemLocation = fromSql location,
    itemAddress = fromSql address,
    itemSellerName = fromSql sellerName,
    itemPrice = fromSql price,
    itemImages = []
  }

imageFromRow :: [SqlValue] -> FinnImage
imageFromRow [finnKode, createdAt, updatedAt, normalUrl, largeUrl] =
  FinnImage {
    imageFinnKode = fromSql finnKode,
    imageCreatedAt = parsePgTimestamp $ fromSql createdAt,
    imageUpdatedAt = parsePgTimestamp $ fromSql updatedAt,
    imageNormalSizeUrl = fromSql normalUrl,
    imageLargeSizeUrl = fromSql largeUrl
  }

parsePgTimestamp :: String -> UTCTime
parsePgTimestamp s = readTime defaultTimeLocale "%s" s :: UTCTime

formatPgTimestamp :: UTCTime -> String
formatPgTimestamp t = formatTime defaultTimeLocale "%F %T" t
