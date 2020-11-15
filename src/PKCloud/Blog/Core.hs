module PKCloud.Blog.Core (
      module Export
    , PKCloudBlog(..)
    , pkBlogRetrievePosts
    , pkBlogRenderContent
    , pkBlogDisplayPreview
    , pkBlogRenderBlog
    , pkBlogRenderDayLong
    , PostLink
    , PostTitle
    , PostPreview
    , PostContent
    , PostPublished
    ) where

import Control.Monad (when)
import qualified Data.Text.Lazy as TextL
import qualified Data.Time.Format as Time
import qualified Text.Markdown as Markdown

import PKCloud.Import
import PKCloud.Blog.Import as Export
import PKCloud.Blog.Routes as Export

-- data PKCloudBlogApp = PKCloudBlogApp

type PostLink = Text
type PostTitle = Text
type PostPreview = Text
-- type PostMarkdown = Text
type PostContent = Text
type PostPublished = Bool
-- type PostEditPublished = Bool

class (SubEntity post, SubEntity tag, PKCloudSecurityPermissions master post, PKCloud master, ToJSON post, FromJSON post) => PKCloudBlog master post tag | master -> post, post -> master, master -> tag, tag -> master where
    -- | Post datatype and getters.
    pkPost :: AuthId master -> PostLink -> UTCTime -> PostPublished -> PostTitle -> PostContent -> PostPreview -> Maybe UTCTime -> Int -> Int -> Int -> post
    pkPostIdField :: EntityField post (Key post)
    pkPostAuthor :: post -> AuthId master -- | Author. Should be an index.
    pkPostAuthorField :: EntityField post (AuthId master)
    pkPostLink :: post -> PostLink
    pkPostYear :: post -> PostYear
    pkPostMonth :: post -> PostMonth
    pkPostDay :: post -> PostDay
    pkPostUniqueLink :: PostYear -> PostMonth -> PostDay -> PostLink -> Unique post
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
    pkPostTagPost :: tag -> Key post -- | Post. Should be an index.
    pkPostTagPostField :: EntityField tag (Key post)
    pkPostTagTag :: tag -> Text -- | Tag. Should be an index.
    pkPostTagTagField :: EntityField tag Text

    -- | Route to link to from a author's name. 
    pkBlogAuthorRoute :: Text -> Route master
    -- pkBlogAuthorRoute = toMasterRoute . PKCloudBlogAuthorR -- Default is that author's posts page.

    -- Active (limit 1 per post?)
    -- Previews?
    --  Create -> Preview -> Edit??
    
-- | Retrieves the n most recent posts.
pkBlogRetrievePosts :: forall site post tag . (PKCloudBlog site post tag) => Int -> HandlerFor site [Entity post]
pkBlogRetrievePosts n = do
    runDB $ select $ from $ \p -> do
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

-- | Render a blog's markdown content. 
pkBlogRenderContent :: Text -> Html
pkBlogRenderContent = Markdown.markdown Markdown.def . TextL.fromStrict

-- | Render a date with a long format.
pkBlogRenderDayLong :: UTCTime -> String
pkBlogRenderDayLong = Time.formatTime Time.defaultTimeLocale "%B %e, %Y"

pkBlogRenderBlog :: forall master post tag . (ToMasterRoute PKCloudBlogApp master, PKCloudBlog master post tag) => post -> [Text] -> WidgetFor master ()
pkBlogRenderBlog post tags = do
    let author :: AuthId master = pkPostAuthor post
    authorName <- handlerToWidget $ pkcloudDisplayName author
    authorIdent <- handlerToWidget $ pkcloudUniqueUsername $ author

    toWidget [lucius|
        .blog-title {
            margin-bottom: 3px;
        }

        .blog-content {
            margin-top: 20px;
            margin-bottom: 35px;
        }
    |]

    [whamlet|
        <h2 .blog-title>
            #{pkPostTitle post}
        <div .text-muted>
            By <a href="@{pkBlogAuthorRoute authorIdent}">#{authorName}</a> - #{pkBlogRenderDayLong $ pkPostDate post}
        <div .blog-content>
            #{pkBlogRenderContent $ pkPostContent post}
        ^{tagsW}
    |]

    where

        toMasterRoute' :: Route PKCloudBlogApp -> Route master
        toMasterRoute' = toMasterRoute

        tagsW = do
            -- Display tags.
            case tags of
                [] ->
                    mempty
                _ -> do
                    let tagsW' = mconcat $ fmap (\tag -> 
                            [whamlet|
                                <li role="presentation">
                                    <a href="@{toMasterRoute' $ PKCloudBlogTagR tag}">
                                        #{tag}
                            |]
                          ) tags
                    tags <- newIdent
                    toWidget [lucius|
                        .#{tags} > li > a {
                            background-color: rgb(246, 246, 246);
                            margin: 0px 10px 10px 0px;
                        }
                    |]
                    [whamlet|
                        <ul .#{tags} .nav .nav-pills>
                            ^{tagsW'}
                    |]

instance PKCloudApp PKCloudBlogApp where
    pkcloudAppName PKCloudBlogApp = "Blog"
    pkcloudAppIdentifier PKCloudBlogApp = "blog"
    pkcloudAppRoot = PKCloudBlogRootR

pkBlogDisplayPreview :: forall site post tag . (PKCloudBlog site post tag, ToMasterRoute PKCloudBlogApp site) => Entity post -> WidgetFor site ()
pkBlogDisplayPreview (Entity _ post) = do
    author <- handlerToWidget $ pkcloudDisplayName $ pkPostAuthor post
    authorIdent <- handlerToWidget $ pkcloudUniqueUsername $ pkPostAuthor post
    let postRouteConst = if pkPostPublished post then
            PKCloudBlogPostR
          else
            PKCloudBlogEditR
    let postRoute = toMasterRoute $ postRouteConst (pkPostYear post) (Int2 $ pkPostMonth post) (Int2 $ pkPostDay post) $ pkPostLink post

    let unpublishedW = when (not $ pkPostPublished post) $ [whamlet|
            <small .unpublished>
                <span .label .label-default>
                    Unpublished
        |]

    [whamlet|
        <div .blog-preview>
            <h3 .blog-title>
                <a href="@{postRoute}">
                    #{pkPostTitle post}
                ^{unpublishedW}
            <div .text-muted>
                By <a href="@{pkBlogAuthorRoute authorIdent}">#{author}</a> - #{pkBlogRenderDayLong $ pkPostDate post}
            <div .blog-content>
                #{pkBlogRenderContent $ pkPostPreview post}
                <p>
                    <a href="@{postRoute}">
                        Continue reading...
        <div .clearfix>
    |]

