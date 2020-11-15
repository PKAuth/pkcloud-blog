module PKCloud.Blog.Handler.Post where

import Import

getPKCloudBlogPostR :: forall site post tag . PostYear -> PostMonth2 -> PostDay2 -> Text -> Handler site post tag Html
getPKCloudBlogPostR year (Int2 month) (Int2 day) slug = liftHandler $ do
    -- Get post.
    (Entity postId post) :: (Entity post) <- runDB $ getBy404 $ pkPostUniqueLink year month day slug

    -- Check if not published.
    when (not $ pkPostPublished post) $ do

        -- Redirect to edit if use is author.
        canEdit <- pkcloudCanWrite post
        if canEdit then
            let route = toMasterRoute $ PKCloudBlogEditR year (Int2 month) (Int2 day) slug :: Route site in
            redirect route
        else
            notFound

    pkcloudDefaultLayout PKCloudBlogApp (pkPostTitle post) $ do
        pkcloudSetTitle $ toHtml $ pkPostTitle post
        sidebarM <- sidebarW post

        tags <- handlerToWidget $ runDB $ select $ from $ \tag -> do
            where_ (tag ^. pkPostTagPostField ==. val postId)
            return $ tag ^. pkPostTagTagField

        let cols = makeColumns sidebarM $ pkBlogRenderBlog post $ fmap unValue tags
        
        [whamlet|
            <div .container>
                <div .row>
                    ^{cols}
        |]

    where
        sidebarW post = do
            -- Check if user can edit post. 
            canEdit <- handlerToWidget $ pkcloudCanWrite post

            if not canEdit then
                return Nothing
            else
                return $ Just [whamlet|
                    <div>
                        <a .btn .btn-default .btn-lg .btn-block href="@{toMasterRoute $ PKCloudBlogEditR year (Int2 month) (Int2 day) $ pkPostLink post}">
                            Edit post
                |]

            -- TODO: Other things in side bar? Recent posts? 

