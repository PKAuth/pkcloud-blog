{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, ConstraintKinds #-}

module Import (
      module Export
    , Handler
--     , Widget
    , MasterWidget
    , MasterForm
    , maybeBlogUserId
    ) where

import Control.Monad as Export (when)
import PKCloud.Import as Export

import PKCloud.Blog.Core as Export
import PKCloud.Blog.Routes as Export

-- TODO: Move to PKCloud.Import?
import Text.Blaze (Markup)
-- | Type for forms in master site.
type MasterForm a = forall site . (RenderMessage site FormMessage) => Markup -> MForm (HandlerT site IO) (FormResult a, WidgetT site IO ())



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

