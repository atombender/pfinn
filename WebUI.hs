{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Control.Monad.IO.Class
import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import Happstack.Server.Response (resp)
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label, toValue, dataAttribute)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value, rel, class_, src)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import FinnTypes
import Storage

main :: IO ()
main = serve Nothing webApp

webApp :: ServerPart Response
webApp = 
  msum [
    dir "assets" $ handleAssets,
    dir "items" $ handleItems,
    dir "read" $ handleMarkRead,
    handleIndex]

  where
    handleIndex :: ServerPart Response
    handleIndex =
      do
        items <- liftIO getItems
        ok $ standardTemplate "Pfinn" $ do
          H.ul ! A.id "items" $ ""
      where
        getItems =
          do
            store <- openStore "fant.db"
            items <- findItems store FindOptions {
              findOptionsOffset = 0,
              findOptionsLimit = 5,
              findOptionsBeforeId = Nothing}
            closeStore store
            return items

    handleItems :: ServerPart Response
    handleItems =
      do
        mBeforeId <- optional $ lookText "beforeId"
        items <- liftIO $ getItemsBefore mBeforeId
        ok $ toResponse $ do
          mapM_ (\item ->
            do
              H.li ! dataAttribute "id" (toValue $ itemFinnKode item) $ do
                H.div ! class_ "meta" $ do
                  ((H.div ! class_ "title") . toHtml . itemTitle) item
                  ((H.div ! class_ "location") . toHtml . itemLocation) item
                (H.div ! class_ "images" $ mapM_ (\image -> do
                  H.img ! src (toValue (imageNormalSizeUrl image))
                  H.span " ") (itemImages item))
            ) items
      where
        getItemsBefore mBeforeId =
          do
            store <- openStore "fant.db"
            items <- findItems store options
            closeStore store
            return items
          where
            options =
              case mBeforeId of
                Nothing -> baseOptions
                Just id -> baseOptions {findOptionsBeforeId = Just $ read (unpack id)}
              where
                baseOptions = FindOptions {
                  findOptionsOffset = 0,
                  findOptionsLimit = 5,
                  findOptionsBeforeId = Nothing
                }

    handleMarkRead :: ServerPart Response
    handleMarkRead =
      do
        id <- lookText "id"
        liftIO $ doMarkRead (show id)
        ok $ toResponse $ do
          H.div ""
      where
        doMarkRead :: String -> IO ()
        doMarkRead id =
          do
            store <- openStore "fant.db"
            markRead store (read id)
            closeStore store
            return ()

    handleAssets :: ServerPart Response
    handleAssets =
      serveDirectory DisableBrowsing [] "./assets"

    standardTemplate :: Text -> Html -> Response
    standardTemplate title body = toResponse $
      H.html $ do
        H.head $ do
          H.title (toHtml title)
          H.link
            ! href "/assets/stylesheets/screen.css"
            ! rel "stylesheet"
            ! A.type_ "text/css"
          (H.script
            ! A.src "/assets/javascripts/vendor/jquery-1.7.1.min.js"
            ! A.type_ "text/javascript") ""
          (H.script
            ! A.src "/assets/javascripts/items.js"
            ! A.type_ "text/javascript") ""
        H.body $ do
          body
