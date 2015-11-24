module PKCloud.Blog.Routes where

import PKCloud.Import

import PKCloud.Blog.Core

-- import PKCloud.Blog.Handler.Posts
-- import PKCloud.Blog.Handler.Root

mkYesodSubData "PKCloudBlogApp" [parseRoutes|
/ PKCloudBlogRootR GET
/posts PKCloudBlogPostsR GET
/posts/#Int64 PKCloudBlogPostsPageR GET
/post/#Text PKCloudBlogPostR GET
/new PKCloudBlogNewR GET POST
/edit/#Text PKCloudBlogEditR GET POST
/delete/#Text PKCloudBlogDeleteR POST
/author/#Text PKCloudBlogAuthorR GET
|]
-- /edits PKCloudBlogEditsR GET

-- routes

-- mkYesodSubData "PKCloudBlog" [parseRoutes|
-- /posts
-- /posts/#page
-- /author/#user
-- /new
-- /edit/#link
-- /post/#link
-- |]
