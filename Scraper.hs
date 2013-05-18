import System.Environment
import System.Exit
import qualified Text.Show.Pretty as Pr

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
    scrapeUrl url =
      do
        putStrLn url
        cache <- openPageCache "fant_cache.db"
        store <- openStore "fant.db"
        feed <- fetchResults cache url
        case feed of
          Left x -> putStrLn x
          Right result -> do
            mapM (saveItem store) (resultItems result)
            return ()
        closePageCache cache
        closeStore store
