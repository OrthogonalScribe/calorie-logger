{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.FoodItems where

import Import

getFoodItemsR :: Handler Html
getFoodItemsR = do
    (_, user) <- requireAuthPair
    entries <- runDB $ selectList [] [Asc FoodItemName]
    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s food items"
        toWidgetBody
            [hamlet|
                <div .ui.container>

                    <h1>
                        Access granted!

                    <p>
                        This page is protected and access is allowed only for authenticated users.

                    <p>
                        MAGWEG-DEBUG: userIdent = <span class="username">#{userIdent user}</span>

                    $if null entries
                        <p>No food items found.
                    $else
                        <ul>
                            $forall Entity _foodItemId item <- entries
                                <li>#{show item}
            |]
