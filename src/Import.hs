{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, ConstraintKinds #-}

module Import (
      module Export
    , Handler
--     , Widget
    , MasterWidget
    , MasterForm
    ) where

import PKCloud.Import as Export

import PKCloud.Blog.Core as Export
import PKCloud.Blog.Routes as Export

-- TODO: Move to PKCloud.Import
import Text.Blaze
-- | Type for forms in master site.
type MasterForm a = forall site . (RenderMessage site FormMessage) => Markup -> MForm (HandlerT site IO) (FormResult a, WidgetT site IO ())



-- type Handler master post edit a = (ToMasterRoute PKCloudBlogApp master, HandlerSite IO ~ master, PKCloudBlog master post edit) => HandlerT PKCloudBlogApp (HandlerT master IO) a
type Handler master post edit a = (ToMasterRoute PKCloudBlogApp master, PKCloudBlog master post edit) => HandlerT PKCloudBlogApp (HandlerT master IO) a
-- type Handler master post edit a = (ToMasterRoute PKCloudBlogApp master, HandlerSite IO ~ master, PKCloudBlog master post edit) => HandlerT PKCloudBlogApp (HandlerT master IO) a

-- type Widget master post edit = (PKCloudBlog master post edit) => WidgetT PKCloudBlogApp (HandlerT master IO) ()
-- type MasterWidget master = forall post edit . (PKCloudBlog master post edit) => WidgetT master IO ()
type MasterWidget master = WidgetT master IO ()

