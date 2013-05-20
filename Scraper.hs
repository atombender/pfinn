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
    mapM_ scrapeUrl args
    exitSuccess
  where
    scrapeUrl :: String -> IO ()
    scrapeUrl url = scrapeUrlOnPage url 1

    scrapeUrlOnPage :: String -> Integer -> IO ()
    scrapeUrlOnPage url pageNumber
      | pageNumber < 10 = (
        do putStrLn ("Scraping " ++ urlWithPage)
           cache <- openPageCache
           store <- openDefaultStore
           feed <- scrapeResults cache urlWithPage
           case feed of
             Left x -> putStrLn x
             Right result -> do
               saved <- liftIO $ mapM (saveItem store) (resultItems result)
               case or saved of
                 True -> scrapeUrlOnPage url (pageNumber + 1)
                 False -> return ()
           closePageCache cache
           closeStore store
      )
      | otherwise = return ()
      where
        urlWithPage
          | pageNumber > 1 = url ++ "&page=" ++ (show pageNumber)
          | otherwise = url
