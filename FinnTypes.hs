module FinnTypes where

import Data.Time (UTCTime)

type FinnKode = String

data FinnImage = FinnImage {imageFinnKode :: FinnKode,
                            imageCreatedAt :: UTCTime,
                            imageUpdatedAt :: UTCTime,
                            imageNormalSizeUrl :: String,
                            imageLargeSizeUrl :: String}
  deriving (Eq, Show, Read)

data FinnItem = FinnItem {itemFinnKode :: FinnKode,
                          itemTitle :: String,
                          itemUrl :: String,
                          itemPublishedAt :: UTCTime,
                          itemCreatedAt :: UTCTime,
                          itemUpdatedAt :: UTCTime,
                          itemLocation :: String,
                          itemPrice :: String,
                          itemImages :: [FinnImage]}
  deriving (Eq, Show, Read)

data FinnResult = FinnResult {resultUrl :: String,
                              resultItems :: [FinnItem]}
  deriving (Eq, Show, Read)
