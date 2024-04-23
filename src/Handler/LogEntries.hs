{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.LogEntries where

import Import
import Yesod.Form.Bootstrap3 (renderBootstrap3, BootstrapFormLayout (..), bfs)
import qualified Database.Esqueleto.Legacy as E
import           Database.Esqueleto.Legacy ((^.))

logEntryForm :: UserId -> Form LogEntry
logEntryForm userId =
    renderBootstrap3 BootstrapBasicForm $ LogEntry
        <$> pure userId
        <*> areq (selectField foodItems) (bfs ("Food item" :: Text)) Nothing
        <*> areq doubleField (bfs ("Weight (g)" :: Text)) Nothing
        <*> lift (liftIO getCurrentTime)
    where
        foodItems = do
            rows <- runDB $ selectList [FoodItemUserId ==. userId] [Asc FoodItemName]
            optionsPairs $ map (\r -> (foodItemName $ entityVal r, entityKey r)) rows

postLogEntriesR :: Handler Html
postLogEntriesR = do
    Entity userId _user <- requireAuth
    ((result, _widget), _enctype) <- runFormPost $ logEntryForm userId
    case result of
        FormSuccess logEntry -> do
            _logEntryId <- runDB $ insert logEntry
            setMessage [shamlet|New log entry created at #{show $ logEntryCreatedAt logEntry}|]
            redirect LogEntriesR
        _ -> defaultLayout $ do
            setMessage "Invalid data entered for new log entry!"
            redirect LogEntriesR

getLogEntriesR :: Handler Html
getLogEntriesR = do
    Entity userId user <- requireAuth
    ((_result, widget), enctype) <- runFormPost $ logEntryForm userId
    rows  <- runDB
        $ E.select
        $ E.from $ \(log_entry `E.InnerJoin` food_item) -> do
            E.on $ log_entry ^. LogEntryFoodItemId E.==. food_item ^. FoodItemId
            E.where_ (log_entry ^. LogEntryUserId E.==. E.val userId)
            E.orderBy [E.asc $ log_entry ^. LogEntryCreatedAt]
            return
                ( food_item ^. FoodItemName
                , log_entry ^. LogEntryWeightG
                , log_entry ^. LogEntryCreatedAt
                )
    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s log entries | Calorie Logger"
        [whamlet|
            <div .ui.container>
                <h1>Log entries

                $if null rows
                    <p>No log entries found.
                $else
                    <table .table>
                        <tr>
                            <th scope="col">Food item
                            <th scope="col">Weight (g)
                            <th scope="col">Creation time

                        $forall (E.Value foodItemName, E.Value weightG, E.Value createdAt) <- rows
                            <tr>
                                <td>#{foodItemName}
                                <td>#{weightG}
                                <td>#{show createdAt}

                <h2>Add new log entry
                <div .row>
                    <div .col-lg-6>
                        <div .bs-callout.bs-callout-info.well>
                            <form .form-horizontal role=form method=post action=@{LogEntriesR} enctype=#{enctype}>
                                ^{widget}
                                <button type="submit" .btn .btn-default>Submit
        |]
