{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module PKCloud.Blog (
      PKCloudBlog(..)
    , module Export
    ) where

import PKCloud.Import


import PKCloud.Blog.Handler.Posts
import PKCloud.Blog.Handler.Root

import PKCloud.Blog.Core as Export
import PKCloud.Blog.Routes as Export

instance (PKCloudBlog master) => YesodSubDispatch PKCloudBlogApp (HandlerT master IO) where
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

