module PKCloud.Blog.Handler.Edit where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL

import Import

generateHTML :: forall site post tag . PostYear -> PostMonth -> PostDay -> post -> (MasterWidget site, Enctype) -> Maybe (post, [Text]) -> Handler site post tag Html
generateHTML year month day post (formW, formEnc) editM = liftHandler $ pkcloudDefaultLayout PKCloudBlogApp "Edit post" $ do
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
                    <a .btn .btn-default .btn-lg .btn-block href="@{toMasterRoute $ PKCloudBlogPostR year (Int2 month) (Int2 day) $ pkPostLink post}">
                        View post
                |]
            else
                mempty

    [whamlet|
        <div .container>
            <div .row>
                <div .col-sm-8>
                    <form role=form method=post action="@{toMasterRoute $ PKCloudBlogEditR year (Int2 month) (Int2 day) $ pkPostLink post}" enctype=#{formEnc}>
                        ^{formW}
                        <div .form-group .optional .pull-right>
                            <button type="submit" name="submit" value="preview" .btn .btn-default>
                                Preview
                            <button type="submit" name="submit" value="update" .btn .btn-primary>
                                Update
                        <div .clearfix>
                    ^{previewW}
                <div .col-sm-4>
                    <button .btn .btn-danger .btn-lg .btn-block data-toggle="modal" data-target="##{deleteModalId}">
                        Delete
                    ^{postLinkW}
                    ^{clearEditsW}
        <div .modal .fade tabindex="-1" role="dialog" id="#{deleteModalId}">
            <div .modal-dialog ##{deleteModalId}-dialog>
                <div .modal-content>
                    <div .modal-body>
                        <h5>
                            Are you sure you want to delete this post?
                        <div ##{deleteModalId}-footer>
                            <button type="button" .btn .btn-default data-dismiss="modal">
                                Cancel
                            <form role=form method=post action="@{toMasterRoute $ PKCloudBlogDeleteR year (Int2 month) (Int2 day) $ pkPostLink post}" enctype=#{deleteE} ##{deleteFormId}>
                                ^{deleteW}
                                <button type="submit" .btn .btn-danger>
                                    Delete
                            ^{clearEditsW}
    |]

    where
        previewW = case editM of
            Nothing -> mempty
            Just (post, tags) -> [whamlet|
                <h1>
                    Preview
                        ^{pkBlogRenderBlog post tags}
            |]

        clearEditsW = case editM of
            Nothing -> return mempty
            Just _ -> do
                (clearW, clearE) <- handlerToWidget $ generateFormPost renderClearEditsForm

                -- JP: Why do we need to add this margin manually?
                clearId <- newIdent
                toWidget [lucius|
                    ##{clearId} {
                        margin-top: 5px;
                    }
                |]

                [whamlet|
                    <form role=form method=post action="@{toMasterRoute $ PKCloudBlogClearEditsR year (Int2 month) (Int2 day) $ pkPostLink post}" enctype=#{clearE}>
                        ^{clearW}
                        <button type="submit" ##{clearId} .btn .btn-default .btn-lg .btn-block>
                            Clear edits
                |]

data FormData = FormData {
      _formDataTitle :: PostTitle
    , _formDataContent :: Textarea
    , _formDataTags :: Maybe [Text]
    , _formDataPublished :: PostPublished
    }

renderEditForm :: forall site post tag . (PKCloudBlog site post tag) => post -> [Text] -> [Text] -> SiteForm site FormData
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

        tagSettings = withAutocapitalizeNone $ withAutocorrectOff $ withPlaceholder "Tags" $
            bfs ("Tags" :: Text)

        withAttr a setting = 
            let oldS = fsAttrs setting in
            setting {fsAttrs = a:oldS}

renderDeleteForm :: SiteForm site ()
renderDeleteForm = renderDivs $ pure ()

renderClearEditsForm :: SiteForm site ()
renderClearEditsForm = renderDivs $ pure ()

generateEditForm :: forall site post tag . Either (Entity post) (post, [Text]) -> Handler site post tag (SiteForm site FormData)

-- Default to pending edits.
generateEditForm (Right (post, tags)) = do
    autoTags <- liftHandler getAutocompleteTags
    return $ renderEditForm post autoTags tags

-- Otherwise, default to existing post.
generateEditForm (Left (Entity postId post)) = do
    autoTags <- liftHandler getAutocompleteTags
    oldTags <- fmap (fmap unValue) $ liftHandler $ runDB $ select $ from $ \tag -> do
        where_ (tag ^. pkPostTagPostField ==. val postId)
        return (tag ^. pkPostTagTagField)
    return $ renderEditForm post autoTags oldTags

getPendingEdits :: forall site post tag . Key post -> Handler site post tag (Maybe (post, [Text]))
getPendingEdits postId = 
    (Aeson.decodeStrict =<<) `fmap` lookupSessionBS ( pkcloudBlogPreviewEditKey postId)

postPKCloudBlogClearEditsR :: forall site post tag . PostYear -> PostMonth2 -> PostDay2 -> PostLink -> Handler site post tag Html
postPKCloudBlogClearEditsR year (Int2 month) (Int2 day) slug = do
    _ <- requireBlogUserId

    -- Lookup post.
    postM :: Maybe (Entity post) <- liftHandler $ runDB $ getBy $ pkPostUniqueLink year month day slug
    case postM of
        Nothing ->
            liftHandler notFound
        Just (Entity postId post) -> do
            -- Check if user can edit.
            hasPermission <- liftHandler $ pkcloudCanWrite post
            when (not hasPermission) $ 
                liftHandler $ permissionDenied "You do not have permission to edit this post."

            -- Delete session edits.
            deleteSession $ pkcloudBlogPreviewEditKey postId

            -- Set message.
            liftHandler $ pkcloudSetMessageSuccess "Cleared edits."

            -- Redirect.
            redirect $ PKCloudBlogEditR year (Int2 month) (Int2 day) slug


getPKCloudBlogEditR :: forall site post tag . PostYear -> PostMonth2 -> PostDay2 -> PostLink -> Handler site post tag Html
getPKCloudBlogEditR year (Int2 month) (Int2 day) slug = do
    _ <- requireBlogUserId

    -- Lookup post.
    postM :: Maybe (Entity post) <- liftHandler $ runDB $ getBy $ pkPostUniqueLink year month day slug
    case postM of
        Nothing ->
            liftHandler notFound
        Just postE@(Entity postId post) -> do
            -- Check if user can edit.
            hasPermission <- liftHandler $ pkcloudCanWrite post
            when (not hasPermission) $ 
                liftHandler $ permissionDenied "You do not have permission to edit this post."

            -- Check if there are edits in the session.
            editM <- getPendingEdits postId

            -- Generate form.
            let postD = maybe (Left postE) Right editM
            form <- generateEditForm postD >>= liftHandler . generateFormPost 

            -- Generate HTML.
            generateHTML year month day post form editM

postPKCloudBlogEditR :: forall site post tag . PostYear -> PostMonth2 -> PostDay2 -> PostLink -> Handler site post tag Html
postPKCloudBlogEditR year (Int2 month) (Int2 day) slug = do
    _ <- requireBlogUserId

    -- Lookup post.
    postM :: Maybe (Entity post) <- liftHandler $ runDB $ getBy $ pkPostUniqueLink year month day slug
    case postM of
        Nothing ->
            liftHandler notFound
        Just postE@(Entity postId post) -> do
            -- Check if user can edit.
            hasPermission <- liftHandler $ pkcloudCanWrite post
            when (not hasPermission) $ 
                liftHandler $ permissionDenied "You do not have permission to edit this post."

            -- Check if there are edits in the session.
            editM <- getPendingEdits postId

            -- Parse form.
            let postD = maybe (Left postE) Right editM
            ((result, formW), formE) <- generateEditForm postD >>= liftHandler . runFormPost
            case result of
                FormMissing -> do
                    liftHandler $ pkcloudSetMessageDanger "Editing post failed."
                    generateHTML year month day post (formW, formE) editM
                FormFailure _msg -> do
                    liftHandler $ pkcloudSetMessageDanger "Editing post failed."
                    generateHTML year month day post (formW, formE) editM
                FormSuccess (FormData title content' tagsM published) -> do
                    -- Update post.
                    let content = unTextarea content'
                    let preview = makeBlogPreview content
                    let author = pkPostAuthor post
                    let date = pkPostDate post
                    let (year, month, day) = splitDate date
                    editDate <- getCurrentTime
                    let newPost = pkPost author slug date published title content preview (Just editDate) year month day

                    let tags = maybe [] id tagsM

                    -- Check if preview was pressed or update.
                    isPreview <- (== Just "preview") `fmap` lookupPostParam "submit"
                    if isPreview then
                        previewPost postId newPost tags
                    else
                        updatePost postId newPost tags

    where
        previewPost postId post tags = do
            -- Save data into cookie.
            setSessionBS (pkcloudBlogPreviewEditKey postId) $ BSL.toStrict $ Aeson.encode (post, tags)
            redirect $ PKCloudBlogEditR year (Int2 month) (Int2 day) slug

        updatePost postId newPost tags = do
            liftHandler $ runDB $ do

                -- Update post.
                replace postId newPost

                -- Delete old tags.
                delete $ from $ \tag -> do
                    where_ (tag ^. pkPostTagPostField ==. val postId)

                -- Insert new tags.
                mapM_ (insert_ . pkPostTag postId) tags

            -- Delete any pending previews.
            deleteSession $ pkcloudBlogPreviewEditKey postId

            -- Set message.
            liftHandler $ pkcloudSetMessageSuccess "Successfully edited post!"

            -- Redirect.
            redirect $ PKCloudBlogEditR year (Int2 month) (Int2 day) slug

pkcloudBlogPreviewEditKey :: PKCloudBlog master post tag => Key post -> Text
pkcloudBlogPreviewEditKey postId = "pkcloud-blog-preview-edit-" <> toPathPiece postId

-- Delete post handler.
postPKCloudBlogDeleteR :: forall site post tag . PostYear -> PostMonth2 -> PostDay2 -> PostLink -> Handler site post tag Html
postPKCloudBlogDeleteR year (Int2 month) (Int2 day) slug = do
    _ <- requireBlogUserId
    -- Lookup post.
    postM :: Maybe (Entity post) <- liftHandler $ runDB $ getBy $ pkPostUniqueLink year month day slug
    case postM of
        Nothing ->
            liftHandler notFound
        Just (Entity postId post) -> do
            -- Check if user can edit.
            hasPermission <- liftHandler $ pkcloudCanWrite post
            when (not hasPermission) $ 
                liftHandler $ permissionDenied "You do not have permission to edit this post."
            
            -- Parse form.
            ((result, _), _) <- liftHandler $ runFormPost renderDeleteForm
            case result of
                FormMissing -> do
                    liftHandler $ pkcloudSetMessageDanger "Deleting post failed."
                    redirect $ PKCloudBlogEditR year (Int2 month) (Int2 day) slug
                FormFailure _msg -> do
                    liftHandler $ pkcloudSetMessageDanger "Deleting post failed."
                    redirect $ PKCloudBlogEditR year (Int2 month) (Int2 day) slug
                FormSuccess () -> do
                    -- Delete post.
                    liftHandler $ runDB $ do
                        delete $ from $ \tag -> do
                            where_ (tag ^. pkPostTagPostField ==. val postId)
                        
                        deleteKey postId

                    -- Set message.
                    liftHandler $ pkcloudSetMessageSuccess "Successfully deleted post."

                    -- Redirect to posts page.
                    redirect PKCloudBlogPostsR

