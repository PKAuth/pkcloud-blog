module PKCloud.Blog.Core where

import PKCloud.Import

data PKCloudBlogApp = PKCloudBlogApp

type PostLink = Text
type PostTitle = Text
type PostPreview = Text
type PostMarkdown = Text
-- type PostContent = Text
type PostPublished = Bool

class (SubEntity (PKPost master), SubEntity (PKPostEdit master), PKCloud master) => PKCloudBlog master where
    -- | Post datatype and getters.
    data PKPost master
    pkPost :: AuthId master -> PostLink -> UTCTime -> PostPublished -> PKPost master
    pkPostIdField :: EntityField (PKPost master) (Key (PKPost master))
    pkPostAuthor :: PKPost master -> AuthId master
    -- pkPostTitle :: PKPost master -> PostTitle
    pkPostLink :: PKPost master -> PostLink
    pkPostDate :: PKPost master -> UTCTime
    pkPostPublished :: PKPost master -> PostPublished
    pkPostPublishedField :: EntityField (PKPost master) PostPublished

    -- | Post edit datatype and getters.
    data PKPostEdit master
    pkPostEdit :: PostTitle -> Key (PKPost master) -> PostMarkdown -> PostPreview -> AuthId master -> UTCTime -> PKPostEdit master
    pkPostEditIdField :: EntityField (PKPostEdit master) (Key (PKPostEdit master))
    pkPostEditTitle :: PKPostEdit master -> PostTitle
    pkPostEditTitleField :: EntityField (PKPostEdit master) PostTitle
    pkPostEditPost :: PKPostEdit master -> Key (PKPost master)
    pkPostEditPostField :: EntityField (PKPostEdit master) (Key (PKPost master))
    pkPostEditContent :: PKPostEdit master -> PostMarkdown
    pkPostEditPreview :: PKPost master -> PostPreview
    pkPostEditDate :: PKPostEdit master -> AuthId master
    pkPostEditDateField :: EntityField (PKPostEdit master) UTCTime
    pkPostEditEditor :: PKPostEdit master -> AuthId master

    -- canPost :: user -> m Bool ???
    
