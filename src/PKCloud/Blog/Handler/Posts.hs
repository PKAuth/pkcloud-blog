module PKCloud.Blog.Handler.Posts (getPKCloudBlogPostsR, getPKCloudBlogPostsPageR) where

import Import

getPostsHelper :: forall site post tag . Int64 -> Handler site post tag Html
getPostsHelper page | page < 1 = getPostsHelper 1
getPostsHelper page = do
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

    -- Check if we should filter unpublished posts.
    queryFilters <- generatePostFilters

    posts <- lift $ runDB $ select $ from $ \p -> do
        where_ $ queryFilters p
        limit qLimit
        offset qOffset
        orderBy [desc (p ^. pkPostDateField)]
        return p

    userM <- maybeBlogUserId

    lift $ pkcloudDefaultLayout PKCloudBlogApp "Blog Posts" $ do
        pkcloudSetTitle "Posts"

        let cols = makeColumns (sidebarW userM) $ displayPostPreviews posts page postsPerPage PKCloudBlogPostsR PKCloudBlogPostsPageR
        [whamlet|
            <div .container>
                <div .row>
                    ^{cols}
        |]

    where 
        postsPerPage :: Int64
        postsPerPage = 10
        qLimit = postsPerPage + 1
        qOffset = (page - 1) * postsPerPage

        sidebarW :: Maybe (AuthId site) -> Maybe (WidgetT site IO ())
        sidebarW userM = 
            let adminW = case userM of
                    Nothing -> 
                        Nothing
                    Just _ -> 
                            -- <div .panel .panel-default>
                            --     <div .panel-body>
                        Just [whamlet|
                            <div>
                                <a .btn .btn-default .btn-lg .btn-block href="@{toMasterRoute PKCloudBlogNewR}">
                                    New post
                        |]
            in
            adminW
            -- [whamlet|
            --     ^{adminW}
            -- |]
            -- TODO: add more like tags? 


getPKCloudBlogPostsR :: Handler site post tag Html
getPKCloudBlogPostsR = getPostsHelper 1

getPKCloudBlogPostsPageR :: Int64 -> Handler site post tag Html
getPKCloudBlogPostsPageR = getPostsHelper
