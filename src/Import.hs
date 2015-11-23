{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, ConstraintKinds #-}

module Import (
      module Export
    , Handler
--     , Widget
    , MasterWidget
    , MasterForm
    , maybeBlogUserId
    , requireBlogUserId
    , makeBlogPreview
    ) where

import Control.Monad as Export (when)
import qualified Data.List as List
import qualified Data.Text as Text
import PKCloud.Import as Export

import PKCloud.Blog.Core as Export
import PKCloud.Blog.Routes as Export

-- TODO: Move to PKCloud.Import?
import Text.Blaze (Markup)
-- | Type for forms in master site.
type MasterForm a = forall site post . (PKCloudBlog site post, RenderMessage site FormMessage) => Markup -> MForm (HandlerT site IO) (FormResult a, WidgetT site IO ())



-- type Handler master post a = (ToMasterRoute PKCloudBlogApp master, PKCloudBlog master post) => HandlerT PKCloudBlogApp (HandlerT master IO) a
type Handler master post a = (ToMasterRoute PKCloudBlogApp master, PKCloudBlog master post) => HandlerT PKCloudBlogApp (HandlerT master IO) a


-- type Widget master post edit = (PKCloudBlog master post edit) => WidgetT PKCloudBlogApp (HandlerT master IO) ()
-- type MasterWidget master = forall post edit . (PKCloudBlog master post edit) => WidgetT master IO ()
type MasterWidget master = WidgetT master IO ()

maybeBlogUserId :: forall site post . Handler site post (Maybe (AuthId site))
maybeBlogUserId = do
    userM <- lift maybeAuthId
    case userM of
        Nothing ->
            return Nothing
        Just userId -> do
            app <- getYesod
            appEnabled <- lift $ pkcloudAppEnabled app userId
            if appEnabled then
                return $ Just userId
            else
                return Nothing

requireBlogUserId :: forall site post . Handler site post (AuthId site)
requireBlogUserId = do
    userId :: AuthId site <- lift requireAuthId

    app <- getYesod
    appEnabled <- lift $ pkcloudAppEnabled app userId
    when (not appEnabled) $
        permissionDenied "You do not have permission to create blog posts. Try enabling the PKCloud blog app."

    return userId

-- | Grab the first three paragraphs as a preview. 
makeBlogPreview :: Text -> Text
makeBlogPreview orig = 
    -- Split into lines.
    let lines' = Text.lines orig in
    
    -- Pull out any leading empty lines. 
    let lines = List.dropWhile isEmpty lines' in

    -- Grab the first three paragraphs.
    let preview = toPreviewHelper 3 lines in

    -- Join preview lines. 
    Text.unlines preview

    where
        toPreviewHelper :: Int -> [Text] -> [Text]
        toPreviewHelper 1 lines = List.takeWhile (not . isEmpty) lines
        toPreviewHelper c lines = 
            let (para, rest') = List.span (not . isEmpty) lines in
            let (spaces, rest) = List.span isEmpty rest' in
            para ++ spaces ++ toPreviewHelper (c - 1) rest

        isEmpty t = "" == Text.strip t


