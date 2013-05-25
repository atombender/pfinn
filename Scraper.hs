{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import System.Exit
import qualified Text.Show.Pretty as Pr
import Control.Monad.IO.Class

import Scraping
import FinnTypes
import PageCache
import Storage

main =
  do
    args <- getArgs
    cache <- openPageCache
    store <- openDefaultStore
    mapM_ (scrapeUrl cache store) args
    closePageCache cache
    closeStore store
    exitSuccess
  where
    scrapeUrl :: PageCache -> Store -> String -> IO ()
    scrapeUrl cache store url = scrapeUrlOnPage cache store url 1

    scrapeUrlOnPage :: PageCache -> Store -> String -> Integer -> IO ()
    scrapeUrlOnPage cache store url pageNumber
      | pageNumber < 30 = do
          putStrLn ("Scraping " ++ urlWithPageNumber)
          feed <- scrapeResults cache urlWithPageNumber
          case feed of
            Left x -> putStrLn x
            Right result -> do
              saved <- liftIO $ mapM (saveItem store) (resultItems result)
              case (and saved) of
                True -> scrapeUrlOnPage cache store url (pageNumber + 1)
                False -> return ()
      | otherwise = return ()
      where
        urlWithPageNumber
          | pageNumber > 1 = url ++ "&page=" ++ (show pageNumber)
          | otherwise = url
