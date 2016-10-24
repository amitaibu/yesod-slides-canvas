module Handler.Socket where

import           Import
import           Yesod.WebSockets

chatStream :: WebSocketsT Handler ()
chatStream = do
    let lesson = toJSON $ Lesson 1 (Just 10)
    sendTextData ("Welcome to the chat server, please enter your name." :: Text)
    name <- receiveData
    sendTextData $ "Welcome, " <> name
    App { appBroadcastChannel = writeChan } <- getYesod
    readChan <- atomically $ do
        writeTChan writeChan $ name <> " has joined the chat"
        dupTChan writeChan

    race_
        (forever $ atomically (readTChan readChan) >>= sendTextData)
        (sourceWS $$ mapM_C (\msg ->
            atomically $ do
                writeTChan writeChan $ name <> ": " <> msg
        ))

getSocketR :: Handler Html
getSocketR = do
    webSockets chatStream
    defaultLayout $ do
          setTitle "WebSockets page"
          toWidget [hamlet|
            Socket's page
          |]
