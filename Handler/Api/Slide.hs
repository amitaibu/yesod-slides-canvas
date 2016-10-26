module Handler.Api.Slide where

import qualified Data.HashMap.Strict as HM
import           Import


addEmbeds :: SlideId
          -> Slide
          -> [Value]
          -> Maybe (HashMap Text Value)
addEmbeds slideId slide embeds =
    case toJSON (Entity slideId slide) of
        Object obj -> Just $ HM.insert "embeds" embeds obj
        _          -> Nothing

valueToHash :: (ToJSON (Entity record), PersistEntity record)
            => Key record
            -> record -> Maybe (HashMap Text Value)
valueToHash key record =
    case toJSON (Entity key record) of
        Object obj -> Just obj
        _          -> Nothing


getSlideR :: SlideId -> Handler Value
getSlideR slideId = do
    slide <- runDB $ get404 slideId

    embedsBySlide <- runDB $ selectList [SlideId ==. slideId] []
    let embeds = [entityIdToJSON (Entity k r) | Entity k r <- embedsBySlide]
    let slideWitheEmbeds = addEmbeds slideId slide embeds

    return $ object ["data" .= slideWitheEmbeds]
