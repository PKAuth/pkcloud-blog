module PKCloud.Blog.Routes where

import PKCloud.Import

import PKCloud.Blog.Core

mkYesodSubData "PKCloudBlog" [parseRoutes|
/ RootsR GET
/posts PostsR GET
|]

-- routes

-- mkYesodSubData "PKCloudBlog" [parseRoutes|
-- /posts
-- /posts/#page
-- /author/#user
-- /new
-- /edit/#link
-- |]
