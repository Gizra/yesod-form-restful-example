#!/usr/bin/env stack
{- stack
     --resolver lts-5.10
     --install-ghc
     runghc
     --package yesod
     --package persistent-sqlite
 -}

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
import Data.Either (lefts)
import Data.Text           (Text)
import Data.Time.Clock
import Database.Persist.Sqlite
import GHC.Generics (Generic)
import Network.HTTP.Types
import Yesod


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
validateNoExistingPrice :: Int -> Handler (Either Text Int)
validateNoExistingPrice price = do
    existing <- runDB $ count [ItemPrice ==. price]
    return $ if (existing > 0)
        then Left "Price already exists"
        else Right price


itemForm :: Html -> MForm Handler (FormResult Item, Widget)
itemForm = renderDivs $ Item
    <$> areq priceField "Price" Nothing
    where priceField = (check validateMinimumPrice . checkM validateNoExistingPrice) intField

-- The GET handler displays the form
getItemR :: Handler Html
getItemR = do
    -- Generate the form to be displayed
    (widget, enctype) <- generateFormPost itemForm
    defaultLayout $ do
        addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.2.4/semantic.min.css"
        [whamlet|
            <div .ui.container>
                <div .ui.raised.segment">
                    <form .ui.form method=post action=@{ItemR} enctype=#{enctype}>
                        ^{widget}
                        <button .ui.button.primary>Submit
        |]


postItemR :: Handler Html
postItemR = do
    ((result, widget), enctype) <- runFormPost itemForm
    renderer <- getUrlRenderParams
    case result of
        FormSuccess item -> do
            -- Insert the entity.
            (Right item') <- insertItem item False
            let (Entity itemId _) = item'
            let html = [hamlet|
                <div .ui.container>
                    <div .message>
                        Item saved! See the <a href="@{ApiItemR itemId}">RESTful</a> item, or <a href="@{ItemR}">back</a> to the form.
                       |]
            setMessage $ toHtml $ html renderer

            defaultLayout $  do
                addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.2.4/semantic.min.css"
                [whamlet|
                    <div .ui.container>
                        <div .ui.raised.segment">
                            <p>#{show item}
                |]
        _ -> do
            let html = [hamlet|
                <div .ui.container>
                    <div .message.errors>
                        Saving failed!
                       |]
            setMessage $ toHtml $ html renderer

            defaultLayout $ do
                addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.2.4/semantic.min.css"
                toWidget [lucius| .errors { color: red; } |]
                toWidget [whamlet|
                    <div .ui.container>
                        <div .ui.raised.segment">
                            <form .ui.form method=post action=@{ItemR} enctype=#{enctype}>
                                ^{widget}
                                <button .ui.button.primary>Submit
                |]


getApiItemR :: ItemId -> Handler Value
getApiItemR itemId = do
    item <- runDB $ get404 itemId
    return $ toJSON item

postApiItemsR :: Handler Value
postApiItemsR = do
    item   <- requireJsonBody :: Handler Item
    mItem <- insertItem item True
    case mItem of
        Left errors -> invalidArgs errors
        Right val -> do
            let (Entity _ entity) = val
            sendResponseStatus status201 (toJSON entity)


insertItem :: Item -> Bool -> Handler (Either [Text] (Entity Item))
insertItem item validate = do
    if validate
        then do
            let validations =
                    [ validateMinimumPrice $ itemPrice item
                    ]

            -- Get the monadic validations.
            validationsM <- sequenceA
                    [ validateNoExistingPrice $ itemPrice item
                    ]

            let lefts' = lefts $ validations ++ validationsM
            if (not $ null lefts')
                then return $ Left lefts'
                else do
                    insertedItem <- runDB $ insertEntity item
                    return $ Right insertedItem
        else do
            insertedItem <- runDB $ insertEntity item
            return $ Right insertedItem


openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "test.db3" openConnectionCount $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
        runMigration migrateAll
        insert $ Item (-10)
    warp 3000 $ App pool
