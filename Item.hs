{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Text           (Text)
import Data.Time.Clock
import Database.Persist.Sqlite
import GHC.Generics (Generic)
import Yesod
import Network.HTTP.Types



data App = App ConnectionPool

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Item
    price Int
    deriving Show Generic
|]


mkYesod "App" [parseRoutes|
/ ItemR GET POST
/api/v1.0/items ApiItemsR POST
/api/v1.0/items/#ItemId ApiItemR GET
|]

instance Yesod App

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend

    runDB action = do
        App pool <- getYesod
        runSqlPool action pool

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance FromJSON Item
instance ToJSON Item

validateMinimumPrice :: Int -> Either Text Int
validateMinimumPrice price =
    if (price <= 0)
        then Left "Price should be above 0"
        else Right price

-- Validate price inside the Handler.
validateMinimumPriceInHandler :: Int -> Handler (Either Text Int)
validateMinimumPriceInHandler price = do
    liftIO $ print  "handler!!!!"
    currentTime <- liftIO getCurrentTime
    return $ if (price <= 0)
        then Left "Price should be above 0, sent from inside Handler"
        else Right price


itemForm :: Html -> MForm Handler (FormResult Item, Widget)
itemForm = renderDivs $ Item
    <$> areq priceField "Price" Nothing
    where priceField = (check validateMinimumPrice . checkM validateMinimumPriceInHandler) intField

-- The GET handler displays the form
getItemR :: Handler Html
getItemR = do
    -- Generate the form to be displayed
    (widget, enctype) <- generateFormPost itemForm
    defaultLayout
        [whamlet|
            <form method=post action=@{ItemR} enctype=#{enctype}>
                ^{widget}
                <button>Submit
        |]


postItemR :: Handler Html
postItemR = do
    ((result, widget), enctype) <- runFormPost itemForm
    case result of
        FormSuccess item ->
            defaultLayout [whamlet|<p>#{show item}|]
        _ -> defaultLayout
            [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{ItemR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]


getApiItemR :: ItemId -> Handler Value
getApiItemR itemId = do
    item <- runDB $ get404 itemId
    return $ toJSON item

postApiItemsR :: Handler Value
postApiItemsR = do
    item   <- requireJsonBody :: Handler Item
    itemId <- runDB $ insert item

    sendResponseStatus status201 $ toJSON item

openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "test.db3" openConnectionCount $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
        runMigration migrateAll
        insert $ Item 50
    warp 3000 $ App pool
