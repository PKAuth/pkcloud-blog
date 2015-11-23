module PKCloud.Blog.Handler.Post where

import Import

getPKCloudBlogPostR :: forall site post . Text -> Handler site post Html
getPKCloudBlogPostR slug = lift $ do
    -- Get post.
    postM :: (Maybe (Entity post)) <- runDB' $ getBy $ pkPostUniqueLink slug
    case postM of
        Nothing ->
            notFound
        Just (Entity _ post) ->
            pkcloudDefaultLayout PKCloudBlogApp $ do
                pkcloudSetTitle $ toHtml $ pkPostTitle post
                let author :: AuthId site = pkPostAuthor post
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
                    <div .container>
                        <div .row>
                            <div .col-sm-8>
                                <h2 .blog-title>
                                    #{pkPostTitle post}
                                <div .text-muted>
                                    By <a href="@{pkBlogAuthorRoute authorIdent}">#{authorName}</a> - #{renderDayLong $ pkPostDate post}
                                <div .blog-content>
                                    #{renderBlogContent $ pkPostContent post}
                            <div .col-sm-4>
                                ^{sidebarW post}
                |]

    where
        sidebarW post = do
            -- Check if user can edit post. 
            canEdit <- handlerToWidget $ pkcloudCanWrite post

            if not canEdit then
                mempty
            else
                [whamlet|
                    <div>
                        <a .btn .btn-default .btn-lg .btn-block href="@{toMasterRoute $ PKCloudBlogEditR $ pkPostLink post}">
                            Edit post
                |]

            -- TODO: Other things in side bar? Recent posts? 

