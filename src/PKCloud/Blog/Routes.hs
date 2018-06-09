module PKCloud.Blog.Routes where

import PKCloud.Import
import PKCloud.Blog.Import

-- import PKCloud.Blog.Core
data PKCloudBlogApp = PKCloudBlogApp

type PostMonth2 = Int2
type PostDay2 = Int2

mkYesodSubData "PKCloudBlogApp" [parseRoutes|
/ PKCloudBlogRootR GET
/posts PKCloudBlogPostsR GET
/posts/#Int64 PKCloudBlogPostsPageR GET
/post/#PostYear/#PostMonth2/#PostDay2/#Text PKCloudBlogPostR GET
/new PKCloudBlogNewR GET POST
/clear/#PostYear/#PostMonth2/#PostDay2/#Text PKCloudBlogClearEditsR POST
/edit/#PostYear/#PostMonth2/#PostDay2/#Text PKCloudBlogEditR GET POST
/delete/#PostYear/#PostMonth2/#PostDay2/#Text PKCloudBlogDeleteR POST
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
