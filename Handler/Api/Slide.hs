module Handler.Api.Slide where

import qualified Data.HashMap.Strict as HM
import           Import


-- addEmbeds :: SlideId
--           -> Slide
--           -> Value
--           -> Maybe (HashMap Text Value)
addEmbeds slideId slide embeds =
    case toJSON (Entity slideId slide) of
        Object obj -> Just $ HM.insert "embeds" (object [("foo", String "Bar")]) obj
        -- Object obj -> Just obj
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
    let slideWitheEmbeds = addEmbeds slideId slide embedsBySlide

    return $ object ["data" .= slideWitheEmbeds]
