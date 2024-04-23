{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.FoodItems where

import Import
import Yesod.Form.Bootstrap3 (renderBootstrap3, BootstrapFormLayout (BootstrapBasicForm), bfs)

foodItemForm :: UserId -> Form FoodItem
foodItemForm userId = renderBootstrap3 BootstrapBasicForm $ FoodItem
    <$> pure userId
    <*> areq textField (bfs ("Name" :: Text)) Nothing
    <*> areq doubleField (bfs ("Kcal" :: Text)) Nothing
    <*> areq doubleField (bfs ("Carbs (g)" :: Text)) Nothing
    <*> areq doubleField (bfs ("Protein (g)" :: Text)) Nothing
    <*> areq doubleField (bfs ("Fat (g)" :: Text)) Nothing

postFoodItemsR :: Handler Html
postFoodItemsR = do
    Entity userId _user <- requireAuth
    ((result, _widget), _enctype) <- runFormPost $ foodItemForm userId
    case result of
        FormSuccess foodItem -> do
            _foodItemId <- runDB $ insert foodItem
            setMessage [shamlet|New food item "#{foodItemName foodItem}" created|]
            redirect FoodItemsR
        _ -> defaultLayout $ do
            setMessage "Invalid data entered for new food item!"
            redirect FoodItemsR

getFoodItemsR :: Handler Html
getFoodItemsR = do
    Entity userId user <- requireAuth
    ((_result, widget), enctype) <- runFormPost $ foodItemForm userId
    entries <- runDB $ selectList [FoodItemUserId ==. userId] [Asc FoodItemName]
    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s food items | Calorie Logger"
        [whamlet|
            <div .ui.container>

                <h1>Food items

                <p>All nutrition data is per 100g

                $if null entries
                    <p>No food items found.
                $else
                    <table .table>
                        <tr>
                            <th scope="col">Name
                            <th scope="col">Kcal
                            <th scope="col">Carbs (g)
                            <th scope="col">Protein (g)
                            <th scope="col">Fat (g)

                        $forall Entity _ item <- entries
                            <tr>
                                <th scope="row">#{foodItemName item}
                                <td>#{foodItemKcal item}
                                <td>#{foodItemCarbs item}
                                <td>#{foodItemProtein item}
                                <td>#{foodItemFat item}

                <h2>Add new food item
                <div .row>
                    <div .col-lg-6>
                        <div .bs-callout.bs-callout-info.well>
                            <form .form-horizontal role=form method=post action=@{FoodItemsR} enctype=#{enctype}>
                                ^{widget}
                                <button type="submit" .btn .btn-default>Submit
        |]
