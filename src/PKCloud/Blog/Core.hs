module PKCloud.Blog.Core where

import PKCloud.Import

data PKCloudBlogApp = PKCloudBlogApp

type PostLink = Text
type PostTitle = Text
type PostMarkdown = Text
type PostContent = Text
type PostPublished = Bool

class YesodAuth master => PKCloudBlog master where
    type PKPost master
    type PKPostEdit master

    pkPost :: AuthId master -> PostLink -> UTCTime -> PostPublished -> PKPost master
    pkPostEdit :: PostTitle -> PostMarkdown -> PostContent -> AuthId master -> UTCTime -> PKPostEdit master

    -- canPost :: user -> m Bool ???
    
