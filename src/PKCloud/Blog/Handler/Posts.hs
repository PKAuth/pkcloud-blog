module PKCloud.Blog.Handler.Posts (getPKCloudBlogPostsR, getPKCloudBlogPostsPageR) where

import Import

import qualified Data.List as List

getPostsHelper :: forall site post . Int64 -> Handler site post Html
-- getPostsHelper :: forall site . (PKCloudBlog site, GeneralPersistSql site (HandlerT site IO)) => Int64 -> HandlerT PKCloudBlogApp (HandlerT site IO) Html
getPostsHelper page | page < 1 = getPostsHelper 1
getPostsHelper page = lift $ do
    -- Get 11 posts. 
    -- posts :: ([Entity (PKPost site)]) <- runDB $ select $ from $ \(InnerJoin p (LeftOuterJoin pe pe')) -> do
    -- posts <- runDB $ select $ from $ \(InnerJoin p (LeftOuterJoin pe (pe' :: SqlExpr (Maybe (Entity (PKPost site)))))) -> do
    --     on (p ^. pkPostIdField ==. pe' ^. pkPostEditPostField &&. 
    --         (just (pe ^. pkPostEditTitleField) <. pe' ?. pkPostEditTitleField ||. just (pe ^. pkPostEditTitleField) ==. pe' ?. pkPostEditTitleField &&. just (pe ^. pkPostEditIdField) <. pe' ?. pkPostEditIdField))
    --     on (p ^. pkPostIdField ==. pe ^. pkPostEditPostField)
    --     where_ ((p ^. pkPostPublishedField ==. val True) &&. (isNothing (pe' ?. pkPostEditIdField)))
    --     limit qLimit
    --     offset qOffset
    --     return (p, pe)

    posts <- runDB' $ select $ from $ \p -> do
        where_ (p ^. pkPostPublishedField ==. val True)
        limit qLimit
        offset qOffset
        orderBy [desc (p ^. pkPostPublishedField)]
        return p

    pkcloudDefaultLayout $ do
        pkcloudSetTitle "Posts"
        case posts of
            [] -> 
                if page == 1 then
                    [whamlet|
                        There are no posts yet. Check back later!
                    |]
                else
                    notFound
            _ -> do
                -- Display up to 10 of their previews.
                let postsW = mconcat $ map displayPreview $ List.take postsPerPage' posts

                [whamlet|
                    ^{postsW}
                    TODO
                |]
                -- Display next/previous buttons.

    where 
        postsPerPage :: Int64
        postsPerPage = 10
        postsPerPage' :: Int
        postsPerPage' = fromInteger $ toInteger postsPerPage
        qLimit = postsPerPage + 1
        qOffset = (page - 1) * postsPerPage

        displayPreview :: Entity post -> WidgetT site IO ()
        displayPreview (Entity _ post) = do
            -- -- Get latest edit.
            -- editL <- handlerToWidget $ runDB' $ select $ from $ \e -> do
            --     where_ (e ^. pkPostEditPostField ==. val postId &&. e ^. pkPostEditPublishedField ==. val True)
            --     limit 1
            --     orderBy [desc (e ^. pkPostEditDateField)]
            --     return e

            -- case editL of
            --     [Entity _editId edit] -> do

            [whamlet|
                <div>
                    <h5>
                        #{pkPostTitle post}
                        <small>
                            Date?? 
                    <hr>
                    <div>
                        link: #{pkPostLink post}
            |]
            --     _ ->
            --         mempty
-- #{pkPostDate post}
        -- displayPreview (Value (Entity _postId post), Value (Entity _editId _edit)) = do
        --     [whamlet|
        --         <div>
        --             #{pkPostLink post}
        --     |]

getPKCloudBlogPostsR :: Handler site post Html
getPKCloudBlogPostsR = getPostsHelper 1

getPKCloudBlogPostsPageR :: Int64 -> Handler site post Html
getPKCloudBlogPostsPageR = getPostsHelper
