{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.LogEntries where

import Import

logEntryForm :: UserId -> Form LogEntry
logEntryForm userId =
    renderDivs $ LogEntry
        <$> pure userId
        <*> areq (selectField foodItems) "Food Item" Nothing
        <*> areq doubleField "weight (g)" Nothing
        <*> lift (liftIO getCurrentTime)
    where
        foodItems = do
            rows <- runDB $ selectList [] [Asc FoodItemName]
            optionsPairs $ map (\r -> (foodItemName $ entityVal r, entityKey r)) rows

postLogEntriesR :: Handler Html
postLogEntriesR = do
    Entity userId _user <- requireAuth
    ((result, _widget), _enctype) <- runFormPost $ logEntryForm userId
    case result of
        FormSuccess logEntry -> do
            _logEntryId <- runDB $ insert logEntry
            setMessage [shamlet|Log Entry created at #{show $ logEntryCreatedAt logEntry}|]
            redirect LogEntriesR
        _ -> defaultLayout $ do
            setMessage "Invalid data entered for new log entry!"
            redirect LogEntriesR

getLogEntriesR :: Handler Html
getLogEntriesR = do
    Entity userId user <- requireAuth
    ((_result, widget), enctype) <- runFormPost $ logEntryForm userId
    entries <- runDB $ selectList [LogEntryUserId ==. userId] [Asc LogEntryCreatedAt]
    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s log entries"
        [whamlet|
            <div .ui.container>

                <h2>Add new log entry
                <form method=post action=@{LogEntriesR} enctype=#{enctype}>
                    ^{widget}
                    <input type=submit>

                <h2>Log Entries
                <p>
                $if null entries
                    <p>No log entries found.
                $else
                    <ul>
                        $forall Entity _logEntryId entry <- entries
                            <li>#{show entry}
        |]
