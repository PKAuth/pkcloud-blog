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

class (SubEntity post, SubEntity tag, PKCloudSecurityPermissions master post, PKCloud master) => PKCloudBlog master post tag | master -> post, post -> master, master -> tag, tag -> master where
    -- | Post datatype and getters.
    pkPost :: AuthId master -> PostLink -> UTCTime -> PostPublished -> PostTitle -> PostContent -> PostPreview -> Maybe UTCTime -> post
    pkPostIdField :: EntityField post (Key post)
    pkPostAuthor :: post -> AuthId master
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

    pkPostTag :: Key post -> Text -> tag
    pkPostTagPost :: tag -> Key post
    pkPostTagPostField :: EntityField tag (Key post)
    pkPostTagTag :: tag -> Text
    pkPostTagTagField :: EntityField tag Text

    -- | Route to link to from a author's name. 
    pkBlogAuthorRoute :: Text -> Route master
    -- pkBlogAuthorRoute = toMasterRoute . PKCloudBlogAuthorR -- Default is that author's posts page.

    -- Active (limit 1 per post?)
    -- Previews?
    --  Create -> Preview -> Edit??
    
-- | Retrieves the n most recent posts.
pkBlogRetrievePosts :: forall site post tag . (PKCloudBlog site post tag) => Int -> HandlerT site IO [Entity post]
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

