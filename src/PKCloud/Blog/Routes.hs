module PKCloud.Blog.Routes where

import PKCloud.Import
import PKCloud.Blog.Import

-- import PKCloud.Blog.Core
data PKCloudBlogApp = PKCloudBlogApp

mkYesodSubData "PKCloudBlogApp" [parseRoutes|
/ PKCloudBlogRootR GET
/posts PKCloudBlogPostsR GET
/posts/#Int64 PKCloudBlogPostsPageR GET
/post/#PostYear/#PostMonth/#PostDay/#Text PKCloudBlogPostR GET
/new PKCloudBlogNewR GET POST
/edit/#PostYear/#PostMonth/#PostDay/#Text PKCloudBlogEditR GET POST
/delete/#PostYear/#PostMonth/#PostDay/#Text PKCloudBlogDeleteR POST
/author/#Text PKCloudBlogAuthorR GET
/author/#Text/#Int64 PKCloudBlogAuthorPageR GET
/tag/#Text PKCloudBlogTagR GET
/tag/#Text/#Int64 PKCloudBlogTagPageR GET
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
