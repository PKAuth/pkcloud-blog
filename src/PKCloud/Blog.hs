{-# LANGUAGE TypeFamilies #-}

module PKCloud.Blog (
      PKCloudBlog(..)
    , module Export
    ) where

import PKCloud.Import

import PKCloud.Blog.Routes as Export

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

class YesodAuth master => PKCloudBlog master where
    type PKPost master
    type PKPostEdit master

    pkPost :: AuthId master -> PostLink -> UTCTime -> PostPublished -> PKPost master
    pkPostEdit :: PostTitle -> PostMarkdown -> PostContent -> AuthId master -> UTCTime -> PKPostEdit master

    -- canPost :: user -> m Bool ???
    


