module PKCloud.Blog.Core where

-- import Control.Monad
import qualified Data.Text.Lazy as TextL
import qualified Data.Time.Format as Time
import qualified Text.Markdown as Markdown

import PKCloud.Import

data PKCloudBlogApp = PKCloudBlogApp

type PostLink = Text
type PostTitle = Text
type PostPreview = Text
-- type PostMarkdown = Text
type PostContent = Text
type PostPublished = Bool
-- type PostEditPublished = Bool

class (SubEntity post, PKCloudSecurityPermissions master post, PKCloud master) => PKCloudBlog master post | master -> post, post -> master where
    -- | Post datatype and getters.
    -- data PKPost master
    pkPost :: AuthId master -> PostLink -> UTCTime -> PostPublished -> PostTitle -> PostContent -> PostPreview -> Maybe UTCTime -> post
    pkPostIdField :: EntityField (post) (Key (post))
    pkPostAuthor :: post -> AuthId master
    -- pkPostTitle :: post -> PostTitle
    pkPostLink :: post -> PostLink
    pkPostUniqueLink :: PostLink -> Unique post
    pkPostDate :: post -> UTCTime
    pkPostDateField :: EntityField post UTCTime
    pkPostPublished :: post -> PostPublished
    pkPostPublishedField :: EntityField (post) PostPublished

    pkPostTitle :: post -> PostTitle
    pkPostTitleField :: EntityField post PostTitle
    pkPostContent :: post -> PostContent
    pkPostPreview :: post -> PostPreview
    pkPostEditDate :: post -> Maybe UTCTime
    pkPostEditDateField :: EntityField post (Maybe UTCTime)

    pkBlogAuthorRoute :: Text -> Route master

    -- | Post edit datatype and getters.
    -- data PKPostEdit master
    -- pkPostEdit :: PostTitle -> Key (post) -> PostMarkdown -> PostPreview -> AuthId master -> UTCTime -> PostEditPublished -> edit
    -- pkPostEditIdField :: EntityField (edit) (Key (edit))
    -- pkPostEditTitle :: edit -> PostTitle
    -- pkPostEditTitleField :: EntityField (edit) PostTitle
    -- pkPostEditPost :: edit -> Key (post)
    -- pkPostEditPostField :: EntityField (edit) (Key (post))
    -- pkPostEditContent :: edit -> PostMarkdown
    -- pkPostEditPreview :: edit -> PostPreview
    -- pkPostEditDate :: edit -> UTCTime
    -- pkPostEditDateField :: EntityField (edit) UTCTime
    -- pkPostEditEditor :: edit -> AuthId master
    -- pkPostEditPublished :: edit -> PostEditPublished
    -- pkPostEditPublishedField :: EntityField edit PostEditPublished
    -- Active (limit 1 per post?)
    -- Previews?
    --  Create -> Preview -> Edit??

    -- canPost :: user -> m Bool ???
    
-- | Retrieves the n most recent posts.
pkBlogRetrievePosts :: forall site post . (PKCloudBlog site post) => Int -> HandlerT site IO [Entity post]
pkBlogRetrievePosts n = do
    runDB' $ select $ from $ \p -> do
        where_ (p ^. pkPostPublishedField ==. val True)
        limit (fromInteger $ toInteger n)
        orderBy [desc (p ^. pkPostDateField)]
        return p

-- | Note: Move to Import.Internal??


    -- foldM (\acc postE@(Entity postId _) -> do
    --     editL <- runDB' $ select $ from $ \e -> do
    --         where_ (e ^. pkPostEditPostField ==. val postId &&. e ^. pkPostEditPublishedField ==. val True)
    --         limit 1
    --         orderBy [desc (e ^. pkPostEditDateField)]
    --         return e
    --     case editL of
    --         [editE] ->
    --             return $ (postE, editE):acc
    --         _ ->
    --             return acc
    --   ) [] posts

renderBlogContent :: Text -> Html
renderBlogContent = Markdown.markdown Markdown.def . TextL.fromStrict

renderDayLong :: UTCTime -> String
renderDayLong = Time.formatTime Time.defaultTimeLocale "%B %e, %Y"

instance PKCloudApp PKCloudBlogApp where
    pkcloudAppName PKCloudBlogApp = "Blog"
    pkcloudAppIdentifier PKCloudBlogApp = "blog"

