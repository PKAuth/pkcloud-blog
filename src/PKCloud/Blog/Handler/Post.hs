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
            pkcloudDefaultLayout $ do
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
                            <div .col-sm-12>
                                <h2 .blog-title>
                                    #{pkPostTitle post}
                                <div .text-muted>
                                    By <a href="@{pkBlogAuthorRoute authorIdent}">#{authorName}</a> - #{renderDayLong $ pkPostDate post}
                                <div .blog-content>
                                    #{renderBlogContent $ pkPostContent post}
                |]

