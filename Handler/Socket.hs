module Handler.Socket where

import           Import
import           Yesod.WebSockets

chatStream :: WebSocketsT Handler ()
chatStream = do
    App { appBroadcastChannel = writeChannel } <- getYesod
    readChannel <- atomically $ dupTChan writeChannel
    race_
        (forever $ atomically (readTChan readChannel) >>= sendTextData)
        (sourceWS $$ mapM_C (atomically . writeTChan writeChannel))

getSocketR :: Handler Html
getSocketR = do
    webSockets chatStream
    defaultLayout $ do
          setTitle "WebSockets page"
          toWidget [hamlet|
            Socket's page
          |]
