module PKCloud.Blog where

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

    
type PostLink = Text
type PostTitle = Text
type PostMarkdown = Text
type PostContent = Text
type PostPublished = Bool

class PKCloudBlog master where
    type PKPost master
    type PKPostEdit master

    pkPost :: user -> PostLink -> UTCTime -> PostPublished -> PKPost master
    pkPostEdit :: PostTitle -> PostMarkdown -> PostContent -> user -> UTCTime -> PKPostEdit master

    -- canPost :: user -> m Bool ???
    


-- routes

mkYesodSubData "PKCloudBlog" [parseRoutes|
/posts
/posts/#page
/author/#user
/new
/edit/#link
|]
