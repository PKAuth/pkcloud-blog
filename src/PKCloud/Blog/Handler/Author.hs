module PKCloud.Blog.Handler.Author (getPKCloudBlogAuthorR, getPKCloudBlogAuthorPageR) where

import Import

getPostsHelper :: Int64 -> Text -> Handler site post tag Html
getPostsHelper page author | page < 1 = getPostsHelper 1 author
getPostsHelper page author = do
    -- Get author ident.
    uIdM <- lift $ pkcloudLookupUniqueUsername author
    case uIdM of
        Nothing ->
            notFound
        Just uId -> do
            -- Check that the author can make posts.
            app <- getYesod
            appEnabled <- lift $ pkcloudAppEnabled app uId
            when (not appEnabled) $
                notFound

            -- Check if we should filter unpublished posts.
            queryFilters <- generatePostFilters

            -- Get posts.
            posts <- lift $ runDB $ select $ from $ \p -> do
                where_ (p ^. pkPostAuthorField ==. val uId &&. queryFilters p)
                limit qLimit
                offset qOffset
                orderBy [desc (p ^. pkPostDateField)]
                return p
            
            username <- lift $ pkcloudDisplayName uId
            let title = "Posts by " <> username
            lift $ pkcloudDefaultLayout PKCloudBlogApp title $ do
                pkcloudSetTitle $ toHtml title

                -- Display previews.
                [whamlet|
                    <div .container>
                        <div .row>
                            <div .col-sm-8>
                                ^{displayPostPreviews posts page postsPerPage (PKCloudBlogAuthorR author) (PKCloudBlogAuthorPageR author)}
                            <div .col-sm-4>
                |]
    where
        postsPerPage :: Int64
        postsPerPage = 10
        qLimit = postsPerPage + 1
        qOffset = (page - 1) * postsPerPage


getPKCloudBlogAuthorR :: forall site post tag . Text -> Handler site post tag Html
getPKCloudBlogAuthorR = getPostsHelper 1

getPKCloudBlogAuthorPageR :: forall site post tag . Text -> Int64 -> Handler site post tag Html
getPKCloudBlogAuthorPageR = flip getPostsHelper


