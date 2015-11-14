module PKCloud.Blog.Core where

import Control.Monad

import PKCloud.Import

data PKCloudBlogApp = PKCloudBlogApp

type PostLink = Text
type PostTitle = Text
type PostPreview = Text
type PostMarkdown = Text
-- type PostContent = Text
type PostPublished = Bool
-- type PostEditPublished = Bool

class (SubEntity (post), SubEntity (edit), PKCloud master) => PKCloudBlog master post edit | master -> post, post -> master, master -> edit, edit -> master where
    -- | Post datatype and getters.
    -- data PKPost master
    pkPost :: AuthId master -> PostLink -> UTCTime -> PostPublished -> post
    pkPostIdField :: EntityField (post) (Key (post))
    pkPostAuthor :: post -> AuthId master
    -- pkPostTitle :: post -> PostTitle
    pkPostLink :: post -> PostLink
    pkPostDate :: post -> UTCTime
    pkPostPublished :: post -> PostPublished
    pkPostPublishedField :: EntityField (post) PostPublished

    -- | Post edit datatype and getters.
    -- data PKPostEdit master
    pkPostEdit :: PostTitle -> Key (post) -> PostMarkdown -> PostPreview -> AuthId master -> UTCTime -> PostEditPublished -> edit
    pkPostEditIdField :: EntityField (edit) (Key (edit))
    pkPostEditTitle :: edit -> PostTitle
    pkPostEditTitleField :: EntityField (edit) PostTitle
    pkPostEditPost :: edit -> Key (post)
    pkPostEditPostField :: EntityField (edit) (Key (post))
    pkPostEditContent :: edit -> PostMarkdown
    pkPostEditPreview :: edit -> PostPreview
    pkPostEditDate :: edit -> UTCTime
    pkPostEditDateField :: EntityField (edit) UTCTime
    pkPostEditEditor :: edit -> AuthId master
    pkPostEditPublished :: edit -> PostEditPublished
    pkPostEditPublishedField :: EntityField edit PostEditPublished
    -- Active (limit 1 per post?)
    -- Previews?
    --  Create -> Preview -> Edit??

    -- canPost :: user -> m Bool ???
    
-- | Retrieves the n most recent posts.
pkBlogRetrievePosts :: forall site post edit . (PKCloudBlog site post edit) => Int -> HandlerT site IO [(Entity post, Entity edit)]
pkBlogRetrievePosts n = do
    posts <- runDB' $ select $ from $ \p -> do
        where_ (p ^. pkPostPublishedField ==. val True)
        limit (fromInteger $ toInteger n)
        orderBy [asc (p ^. pkPostPublishedField)]
        return p
    foldM (\acc postE@(Entity postId _) -> do
        editL <- runDB' $ select $ from $ \e -> do
            where_ (e ^. pkPostEditPostField ==. val postId &&. e ^. pkPostEditPublishedField ==. val True)
            limit 1
            orderBy [desc (e ^. pkPostEditDateField)]
            return e
        case editL of
            [editE] ->
                return $ (postE, editE):acc
            _ ->
                return acc
      ) [] posts
