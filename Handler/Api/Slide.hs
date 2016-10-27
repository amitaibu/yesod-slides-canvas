module Handler.Api.Slide where

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V
import           Import


getSlideR :: SlideId -> Handler Value
getSlideR slideId = do
    slide <- runDB $ get404 slideId

    let (Object slideHM) = entityIdToJSON (Entity slideId slide)

    embedTextsBySlide <- runDB $ selectList [EmbedTextSlide ==. slideId] [] :: Handler [Entity EmbedText]
    embedImagesBySlide <- runDB $ selectList [EmbedImageSlide ==. slideId] [] :: Handler [Entity EmbedImage]

    let embeds = Array $ V.fromList [entityIdToJSON (Entity k r) | Entity k r <- embedTextsBySlide]

    let slideWitheEmbeds = HM.insert "embeds" embeds slideHM

    return $ object ["data" .= slideWitheEmbeds]
