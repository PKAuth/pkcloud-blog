module PKCloud.Blog.Handler.Root where

import qualified Database.Esqueleto as E

import Import

getPKCloudBlogRootR :: forall site post tag . Handler site post tag Html
getPKCloudBlogRootR = do -- redirect PKCloudBlogPostsR
    
    -- Check if we should filter unpublished posts.
    queryFilters <- generatePostFilters

    userM <- maybeBlogUserId
    -- JP: Redirect if user isn't logged in?

    let title = pkcloudAppName PKCloudBlogApp
    lift $ pkcloudDefaultLayout PKCloudBlogApp title $ do
        pkcloudSetTitle $ toHtml title

        toWidget [lucius|
            .blog-h2 a {
                color: rgb(51, 51, 51);
            }
        |]

        [whamlet|
            <div .container>
                <div .row>
                    ^{authorPostsW userM queryFilters}

                <div .row>
                    ^{recentPostsW queryFilters}
        |]

        -- TODO: Popular tags?

    where

        authorPostsW :: Maybe (AuthId site) -> (SqlExpr (Entity post) -> SqlExpr (E.Value Bool)) -> WidgetT site IO ()
        authorPostsW Nothing _ = return ()
        authorPostsW (Just uId) queryFilters = do
            
            -- Get author's recent posts.
            posts <- handlerToWidget $ runDB $ select $ from $ \p -> do
                where_ (p ^. pkPostAuthorField ==. val uId &&. queryFilters p)
                limit qLimit
                orderBy [desc (p ^. pkPostDateField)]
                return p

            let sidebar = Just [whamlet|
                <div>
                    <a .btn .btn-default .btn-lg .btn-block href="@{toMasterRoute PKCloudBlogNewR}">
                        New post
            |]

            authorIdent <- handlerToWidget $ pkcloudUniqueUsername uId
            makeColumns sidebar $ [whamlet|
                <h2 .blog-h2>
                    <a href="@{toMasterRoute $ PKCloudBlogAuthorR authorIdent}">
                        Your Posts
                ^{displayPostPreviews posts 1 dLimit (PKCloudBlogAuthorR authorIdent) (PKCloudBlogAuthorPageR authorIdent)}
            |]

        recentPostsW :: (SqlExpr (Entity post) -> SqlExpr (E.Value Bool)) -> WidgetT site IO ()
        recentPostsW queryFilters = do
            
            -- Get recent posts.
            posts <- handlerToWidget $ runDB $ select $ from $ \p -> do
                  where_ $ queryFilters p
                  limit qLimit
                  orderBy [desc (p ^. pkPostDateField)]
                  return p

            [whamlet|
                <div .col-xs-12>
                    <h2 .blog-h2>
                        <a href="@{toMasterRoute PKCloudBlogPostsR}">
                            Recent Posts
                    ^{displayPostPreviews posts 1 dLimit PKCloudBlogPostsR PKCloudBlogPostsPageR}
            |]

        dLimit = 5
        qLimit = 5 -- dLimit + 1

