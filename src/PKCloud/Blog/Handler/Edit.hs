module PKCloud.Blog.Handler.Edit where

import Import

generateHTML :: forall site post tag . post -> (MasterWidget site, Enctype) -> Handler site post tag Html
generateHTML post (formW, formEnc) = lift $ pkcloudDefaultLayout PKCloudBlogApp $ do
    pkcloudSetTitle "Edit post"

    -- Make delete form.
    (deleteW, deleteE) <- handlerToWidget $ generateFormPost renderDeleteForm

    deleteModalId <- newIdent
    deleteFormId <- newIdent
    toWidget [lucius|
        ##{deleteModalId}-dialog {
            width: 40%;
            min-width: 350px;
            margin-left: auto;
            margin-right: auto;
        }

        ##{deleteModalId}-footer {
            text-align: right;
            padding-top: 15px;
        }
        ##{deleteFormId} {
            display: inline-block;
        }
    |]
    let postLinkW = 
            if pkPostPublished post then
                [whamlet|
                    <a .btn .btn-default .btn-lg .btn-block href="@{toMasterRoute $ PKCloudBlogPostR $ pkPostLink post}">
                        View post
                |]
            else
                mempty

    [whamlet|
        <div .container>
            <div .row>
                <div .col-sm-8>
                    <form role=form method=post action="@{toMasterRoute $ PKCloudBlogEditR $ pkPostLink post}" enctype=#{formEnc}>
                        ^{formW}
                        <div .form-group .optional .pull-right>
                            <a href="#" name="preview" .btn .btn-default>
                                Preview
                            <button type="submit" name="update" .btn .btn-primary>
                                Update
                        <div .clearfix>
                <div .col-sm-4>
                    <button .btn .btn-danger .btn-lg .btn-block data-toggle="modal" data-target="##{deleteModalId}">
                        Delete
                    ^{postLinkW}
        <div .modal .fade tabindex="-1" role="dialog" id="#{deleteModalId}">
            <div .modal-dialog ##{deleteModalId}-dialog>
                <div .modal-content>
                    <div .modal-body>
                        <h5>
                            Are you sure you want to delete this post?
                        <div ##{deleteModalId}-footer>
                            <button type="button" .btn .btn-default data-dismiss="modal">
                                Cancel
                            <form role=form method=post action="@{toMasterRoute $ PKCloudBlogDeleteR $ pkPostLink post}" enctype=#{deleteE} ##{deleteFormId}>
                                ^{deleteW}
                                <button type="submit" .btn .btn-danger>
                                    Delete
    |]

data FormData = FormData {
      _formDataTitle :: PostTitle
    , _formDataContent :: Textarea
    , _formDataTags :: Maybe [Text]
    , _formDataPublished :: PostPublished
    }

renderEditForm :: forall site post tag . (PKCloudBlog site post tag) => post -> [Text] -> [Text] -> MasterForm FormData
renderEditForm post tags oldTags = renderBootstrap3 BootstrapBasicForm $ FormData
    <$> areq textField titleSettings ( Just $ pkPostTitle post)
    <*> areq textareaField contentSettings ( Just $ Textarea $ pkPostContent post)
    <*> aopt (tagField tags) tagSettings (Just (Just oldTags))
    <*> areq (bootstrapCheckBoxField ("Publish" :: Text)) publishSettings ( Just $ pkPostPublished post)

    where
        titleSettings = withAutofocus $ withPlaceholder "Title" $ 
            bfs ("Title" :: Text)

        contentSettings = withAttr ("rows","10") $ withPlaceholder "Content" $ 
            bfs ("Content" :: Text)

        publishSettings = "Publish"

        tagSettings = withPlaceholder "Tags" $
            bfs ("Tags" :: Text)

        withAttr a setting = 
            let oldS = fsAttrs setting in
            setting {fsAttrs = a:oldS}

renderDeleteForm :: MasterForm ()
renderDeleteForm = renderDivs $ pure ()

renderEditHelper :: forall site post tag . Key post -> Handler site post tag ([Text], [Text])
renderEditHelper postId = do
    tags <- lift $ getAutocompleteTags
    oldTags <- fmap (fmap unValue) $ lift $ runDB' $ select $ from $ \tag -> do
        where_ (tag ^. pkPostTagPostField ==. val postId)
        return (tag ^. pkPostTagTagField)
    return (tags, oldTags)

getPKCloudBlogEditR :: forall site post tag . PostLink -> Handler site post tag Html
getPKCloudBlogEditR slug = do
    _ <- requireBlogUserId

    -- Lookup post.
    postM :: Maybe (Entity post) <- lift $ runDB' $ getBy $ pkPostUniqueLink slug
    case postM of
        Nothing ->
            lift notFound
        Just (Entity postId post) -> do
            -- Check if user can edit.
            hasPermission <- lift $ pkcloudCanWrite post
            when (not hasPermission) $ 
                lift $ permissionDenied "You do not have permission to edit this post."
            
            -- Generate form.
            (autoTags, oldTags) <- renderEditHelper postId
            form <- lift $ generateFormPost $ renderEditForm post autoTags oldTags

            -- Generate HTML.
            generateHTML post form

postPKCloudBlogEditR :: forall site post tag . PostLink -> Handler site post tag Html
postPKCloudBlogEditR slug = do
    _ <- requireBlogUserId

    -- Lookup post.
    postM :: Maybe (Entity post) <- lift $ runDB' $ getBy $ pkPostUniqueLink slug
    case postM of
        Nothing ->
            lift notFound
        Just (Entity postId post) -> do
            -- Check if user can edit.
            hasPermission <- lift $ pkcloudCanWrite post
            when (not hasPermission) $ 
                lift $ permissionDenied "You do not have permission to edit this post."
            
            -- Parse form.
            (autoTags, oldTags) <- renderEditHelper postId
            ((result, formW), formE) <- lift $ runFormPost $ renderEditForm post autoTags oldTags
            case result of
                FormMissing -> do
                    lift $ pkcloudSetMessageDanger "Editing post failed."
                    generateHTML post (formW, formE)
                FormFailure _msg -> do
                    lift $ pkcloudSetMessageDanger "Editing post failed."
                    generateHTML post (formW, formE)
                FormSuccess (FormData title content' tagsM published) -> do
                    -- Update post.
                    let content = unTextarea content'
                    let preview = makeBlogPreview content
                    let author = pkPostAuthor post
                    let date = pkPostDate post
                    editDate <- getCurrentTime
                    let newPost = pkPost author slug date published title content preview (Just editDate)
                    lift $ runDB' $ do
                        -- Update post.
                        replace postId newPost

                        -- Delete old tags.
                        delete $ from $ \tag -> do
                            where_ (tag ^. pkPostTagPostField ==. val postId)

                        -- Insert new tags.
                        let tags = maybe [] id tagsM
                        mapM_ (insert_ . pkPostTag postId) tags

                    -- Set message.
                    lift $ pkcloudSetMessageSuccess "Successfully edited post!"

                    -- Redirect.
                    redirect $ PKCloudBlogEditR slug

-- Delete post handler.
postPKCloudBlogDeleteR :: forall site post tag . PostLink -> Handler site post tag Html
postPKCloudBlogDeleteR slug = do
    _ <- requireBlogUserId
    -- Lookup post.
    postM :: Maybe (Entity post) <- lift $ runDB' $ getBy $ pkPostUniqueLink slug
    case postM of
        Nothing ->
            lift notFound
        Just (Entity postId post) -> do
            -- Check if user can edit.
            hasPermission <- lift $ pkcloudCanWrite post
            when (not hasPermission) $ 
                lift $ permissionDenied "You do not have permission to edit this post."
            
            -- Parse form.
            ((result, _), _) <- lift $ runFormPost renderDeleteForm
            case result of
                FormMissing -> do
                    lift $ pkcloudSetMessageDanger "Deleting post failed."
                    redirect $ PKCloudBlogEditR slug
                FormFailure _msg -> do
                    lift $ pkcloudSetMessageDanger "Deleting post failed."
                    redirect $ PKCloudBlogEditR slug
                FormSuccess () -> do
                    -- Delete post.
                    lift $ runDB' $ do
                        delete $ from $ \tag -> do
                            where_ (tag ^. pkPostTagPostField ==. val postId)
                        
                        deleteKey postId

                    -- Set message.
                    lift $ pkcloudSetMessageSuccess "Successfully deleted post."

                    -- Redirect to posts page.
                    redirect PKCloudBlogPostsR

