module PKCloud.Blog.Handler.Posts (getPostsR, getPostsPageR) where

import Import

import qualified Data.List as List

getPostsHelper :: forall site . Int64 -> Handler site Html
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
        return p

    pkcloudDefaultLayout $ case posts of
        [] -> 
            [whamlet|
                There are no posts yet.
            |]
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
        qOffset = (page - 1) * postsPerPage - 1

        displayPreview :: Entity (PKPost site) -> WidgetT site IO ()
        displayPreview (Entity postId post) = do
            -- Get latest edit.
            editL <- handlerToWidget $ runDB' $ select $ from $ \e -> do
                where_ (e ^. pkPostEditPostField ==. val postId)
                limit 1
                orderBy [desc (e ^. pkPostEditDateField)]
                return e

            case editL of
                [Entity _editId edit] -> do

                    [whamlet|
                        <div>
                            <h5>
                                #{pkPostEditTitle edit}
                                <small>
                                    Date?? 
                            <hr>
                            <div>
                                link: #{pkPostLink post}
                    |]
                _ ->
                    mempty
-- #{pkPostDate post}
        -- displayPreview (Value (Entity _postId post), Value (Entity _editId _edit)) = do
        --     [whamlet|
        --         <div>
        --             #{pkPostLink post}
        --     |]

getPostsR :: Handler site Html
getPostsR = getPostsHelper 1

getPostsPageR :: Int64 -> Handler site Html
getPostsPageR = getPostsHelper
