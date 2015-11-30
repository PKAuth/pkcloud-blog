{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

-- | This module typically is imported by end user sites.

module PKCloud.Blog (
      PKCloudBlog(..)
    , module Export
    ) where

import PKCloud.Import

import PKCloud.Blog.Handler.Author
import PKCloud.Blog.Handler.Edit
import PKCloud.Blog.Handler.New
import PKCloud.Blog.Handler.Post
import PKCloud.Blog.Handler.Posts
import PKCloud.Blog.Handler.Root
import PKCloud.Blog.Handler.Tag

import PKCloud.Blog.Core as Export
import PKCloud.Blog.Routes as Export

instance (ToMasterRoute PKCloudBlogApp master, PKCloudBlog master post tag) => YesodSubDispatch PKCloudBlogApp (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesPKCloudBlogApp)

-- BlogPost
--     acl AccessControlList
--     author user..
--     link Text
--     date UTCTime
--     published Bool
--     UniqueBlogPostLink link
-- 
-- BlogPostEdit
--     title Text
--     content Text
--     html Html
--     editor user..
--     date UTCTime

