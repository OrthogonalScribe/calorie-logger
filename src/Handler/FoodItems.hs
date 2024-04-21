{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.FoodItems where

import Import

foodItemForm :: UserId -> Form FoodItem
foodItemForm userId = renderDivs $ FoodItem
    <$> pure userId
    <*> areq textField "Name" Nothing
    <*> areq doubleField "Kcal/100g" Nothing
    <*> areq doubleField "carbs/100g" Nothing
    <*> areq doubleField "protein/100g" Nothing
    <*> areq doubleField "fat/100g" Nothing

postFoodItemsR :: Handler Html
postFoodItemsR = do
    Entity userId _user <- requireAuth
    ((result, _widget), _enctype) <- runFormPost $ foodItemForm userId
    case result of
        FormSuccess foodItem -> do
            _foodItemId <- runDB $ insert foodItem
            setMessage [shamlet|Food item #{foodItemName foodItem} created|]
            redirect FoodItemsR
        _ -> defaultLayout $ do
            setMessage "Invalid data entered for new food item!"
            redirect FoodItemsR

getFoodItemsR :: Handler Html
getFoodItemsR = do
    Entity userId user <- requireAuth
    ((_result, widget), enctype) <- runFormPost $ foodItemForm userId
    entries <- runDB $ selectList [] [Asc FoodItemName]
    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s food items"
        [whamlet|
            <div .ui.container>

                <h1>
                    Access granted!

                <p>
                    This page is protected and access is allowed only for authenticated users.

                <p>
                    MAGWEG-DEBUG: userIdent = <span class="username">#{userIdent user}</span>

                <h2>Add new food item
                <form method=post action=@{FoodItemsR} enctype=#{enctype}>
                    ^{widget}
                    <input type=submit>

                <h2>Food items
                <p>
                $if null entries
                    <p>No food items found.
                $else
                    <ul>
                        $forall Entity _foodItemId item <- entries
                            <li>#{show item}
        |]
