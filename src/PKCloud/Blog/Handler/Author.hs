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

                -- Make columns.
                isAuthor <- isAuthorW uId
                let sidebar = sidebarM isAuthor
                let cols = makeColumns sidebar $
                      -- Display previews.
                      displayPreviews isAuthor posts page postsPerPage (PKCloudBlogAuthorR author) (PKCloudBlogAuthorPageR author)

                [whamlet|
                    <div .container>
                        <div .row>
                            ^{cols}
                |]
    where
        -- If current user is the author, show the previews as the author.
        displayPreviews True = displayPostPreviewsAsAuthor
        displayPreviews False = displayPostPreviews

        postsPerPage :: Int64
        postsPerPage = 10
        qLimit = postsPerPage + 1
        qOffset = (page - 1) * postsPerPage

        isAuthorW authorId = do
            userIdM <- handlerToWidget maybeAuthId
            return $ userIdM == Just authorId

        -- If current user is the author, show the new post button.
        sidebarM True = Just [whamlet|
                <div>
                    <a .btn .btn-default .btn-lg .btn-block href="@{toMasterRoute PKCloudBlogNewR}">
                        New post
            |]
        sidebarM False = Nothing


getPKCloudBlogAuthorR :: forall site post tag . Text -> Handler site post tag Html
getPKCloudBlogAuthorR = getPostsHelper 1

getPKCloudBlogAuthorPageR :: forall site post tag . Text -> Int64 -> Handler site post tag Html
getPKCloudBlogAuthorPageR = flip getPostsHelper


