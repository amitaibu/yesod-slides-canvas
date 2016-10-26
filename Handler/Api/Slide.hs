module Handler.Api.Slide where

import           Import

getSlideR :: SlideId -> Handler Value
getSlideR slideId = do
    slide <- runDB $ get404 slideId

    return $ object ["data" .= slide]
