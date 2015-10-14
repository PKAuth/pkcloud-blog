{-# LANGUAGE TypeFamilies #-}

module PKCloud.Blog (
      PKCloudBlog(..)
    , module Export
    ) where

import PKCloud.Import

import PKCloud.Blog.Core as Export
import PKCloud.Blog.Routes as Export

import PKCloud.Blog.Handler.Posts
import PKCloud.Blog.Handler.Root

instance (PKCloudBlog master, Yesod master) => YesodSubDispatch PKCloudBlogApp (HandlerT master IO) where
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

