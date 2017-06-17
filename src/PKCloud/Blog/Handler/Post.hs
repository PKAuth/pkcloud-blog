module PKCloud.Blog.Handler.Post where

import Import

getPKCloudBlogPostR :: forall site post tag . PostYear -> PostMonth -> PostDay -> Text -> Handler site post tag Html
getPKCloudBlogPostR year month day slug = lift $ do
    -- Get post.
    postM :: (Maybe (Entity post)) <- runDB $ getBy $ pkPostUniqueLink year month day slug
    case postM of
        Nothing ->
            notFound
        Just (Entity postId post) -> do
            -- Check if not published.
            when (not $ pkPostPublished post) 
                notFound

            pkcloudDefaultLayout PKCloudBlogApp (pkPostTitle post) $ do
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
                sidebarM <- sidebarW post
                let cols = makeColumns sidebarM [whamlet|
                    <h2 .blog-title>
                        #{pkPostTitle post}
                    <div .text-muted>
                        By <a href="@{pkBlogAuthorRoute authorIdent}">#{authorName}</a> - #{pkBlogRenderDayLong $ pkPostDate post}
                    <div .blog-content>
                        #{pkBlogRenderContent $ pkPostContent post}
                    ^{tagsW postId}
                |]
                [whamlet|
                    <div .container>
                        <div .row>
                            ^{cols}
                |]

    where
        tagsW postId = do
            -- Get tags.
            tags <- handlerToWidget $ runDB $ select $ from $ \tag -> do
                where_ (tag ^. pkPostTagPostField ==. val postId)
                return tag
            
            case tags of
                [] ->
                    mempty
                _ -> do
                    let tagsW' = mconcat $ fmap (\(Entity _ tag) -> 
                                [whamlet|
                                    <li role="presentation">
                                        <a href="@{toMasterRoute $ PKCloudBlogTagR $ pkPostTagTag tag}">
                                            #{pkPostTagTag tag}
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

        sidebarW post = do
            -- Check if user can edit post. 
            canEdit <- handlerToWidget $ pkcloudCanWrite post

            if not canEdit then
                return Nothing
            else
                return $ Just [whamlet|
                    <div>
                        <a .btn .btn-default .btn-lg .btn-block href="@{toMasterRoute $ PKCloudBlogEditR year month day $ pkPostLink post}">
                            Edit post
                |]

            -- TODO: Other things in side bar? Recent posts? 

