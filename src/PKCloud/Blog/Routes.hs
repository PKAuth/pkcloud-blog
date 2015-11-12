module PKCloud.Blog.Routes where

import PKCloud.Import

import PKCloud.Blog.Core

-- import PKCloud.Blog.Handler.Posts
-- import PKCloud.Blog.Handler.Root

mkYesodSubData "PKCloudBlogApp" [parseRoutes|
/ PKCloudBlogRootR GET
/posts PKCloudBlogPostsR GET
/posts/#Int64 PKCloudBlogPostsPageR GET
/new PKCloudBlogNewR GET POST
|]

-- routes

-- mkYesodSubData "PKCloudBlog" [parseRoutes|
-- /posts
-- /posts/#page
-- /author/#user
-- /new
-- /edit/#link
-- /post/#link
-- |]
