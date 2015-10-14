module PKCloud.Blog.Routes where

import PKCloud.Blog.Core
import PKCloud.Import

-- import PKCloud.Blog.Handler.Posts
-- import PKCloud.Blog.Handler.Root

mkYesodSubData "PKCloudBlogApp" [parseRoutes|
/ RootR GET
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
