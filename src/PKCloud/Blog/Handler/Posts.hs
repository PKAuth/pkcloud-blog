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
        orderBy [desc (p ^. pkPostDateField)]
        return p

    pkcloudDefaultLayout $ do
        pkcloudSetTitle "Posts"
        wrap $ case posts of
            [] -> 
                if page == 1 then
                    [whamlet|
                        <div .container>
                            <div .row>
                                <div .col-sm-12>
                                    There are no posts yet. Check back later!
                    |]
                else
                    notFound
            _ -> do
                -- Display up to 10 of their previews.
                let postsW = mconcat $ map displayPreview $ List.take postsPerPage' posts

                [whamlet|
                    <div .container>
                        ^{postsW}
                        <div .row>
                            <div .col-sm-12>
                                ^{navigationW page posts}
                |]
                -- Display next/previous buttons.

    where 
        wrap w = [whamlet|
                            ^{w}
            |]
        postsPerPage :: Int64
        postsPerPage = 10
        postsPerPage' :: Int
        postsPerPage' = fromInteger $ toInteger postsPerPage
        qLimit = postsPerPage + 1
        qLimit' = postsPerPage' + 1
        qOffset = (page - 1) * postsPerPage

        navigationW :: Int64 -> [a] -> WidgetT site IO ()
        navigationW page l = do
            -- Check if we should display the older button.
            let masterPostsRoute = toMasterRoute . PKCloudBlogPostsPageR
            let older = if List.length l == qLimit' then
                    [whamlet|
                        <ul .nav .nav-pills .pull-right>
                            <li role="presentation">
                                <a href="@{masterPostsRoute (page + 1)}">
                                    Older
                    |]
                  else
                    mempty

            -- Check if we should display the newer button.
            let newer = case page of
                    2 ->
                        [whamlet|
                            <ul .nav .nav-pills>
                                <li role="presentation">
                                    <a href="@{toMasterRoute PKCloudBlogPostsR}">
                                        Newer
                        |]
                    1 -> 
                        mempty
                    _ ->
                        [whamlet|
                            <ul .nav .nav-pills>
                                <li role="presentation">
                                    <a href="@{masterPostsRoute (page - 1)}">
                                        Newer
                        |]

            toWidget [lucius|
                .blog-title {
                    margin-bottom: 3px;
                }

                .blog-title a {
                    color: rgb(51, 51, 51);
                }

                .blog-content {
                    margin-top: 15px;
                    margin-bottom: 35px;
                }
            |]

            [whamlet|
                <div>
                    ^{newer}
                    ^{older}
                <div .clearfix>
            |]

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

            author <- handlerToWidget $ pkcloudDisplayName $ pkPostAuthor post
            authorIdent <- handlerToWidget $ pkcloudUniqueUsername $ pkPostAuthor post
            let postRoute = toMasterRoute $ PKCloudBlogPostR $ pkPostLink post
            [whamlet|
                <div .row>
                    <div .col-sm-12>
                        <h3 .blog-title>
                            <a href="@{postRoute}">
                                #{pkPostTitle post}
                        <div .text-muted>
                            By <a href="@{pkBlogAuthorRoute authorIdent}">#{author}</a> - #{renderDayLong $ pkPostDate post}
                        <div .blog-content>
                            #{renderBlogContent $ pkPostPreview post}
                            <p>
                                <a href="@{postRoute}">
                                    Continue reading...
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
