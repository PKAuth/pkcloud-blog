module PKCloud.Blog.Handler.Edit where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL

import Import

generateHTML :: forall site post tag . PostYear -> PostMonth -> PostDay -> post -> (MasterWidget site, Enctype) -> Handler site post tag Html
generateHTML year month day post (formW, formEnc) = lift $ pkcloudDefaultLayout PKCloudBlogApp "Edit post" $ do
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
                    <a .btn .btn-default .btn-lg .btn-block href="@{toMasterRoute $ PKCloudBlogPostR year month day $ pkPostLink post}">
                        View post
                |]
            else
                mempty

    [whamlet|
        <div .container>
            <div .row>
                <div .col-sm-8>
                    <form role=form method=post action="@{toMasterRoute $ PKCloudBlogEditR year month day $ pkPostLink post}" enctype=#{formEnc}>
                        ^{formW}
                        <div .form-group .optional .pull-right>
                            <button type="submit" name="submit" value="preview" .btn .btn-default>
                                Preview
                            <button type="submit" name="submit" value="update" .btn .btn-primary>
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
                            <form role=form method=post action="@{toMasterRoute $ PKCloudBlogDeleteR year month day $ pkPostLink post}" enctype=#{deleteE} ##{deleteFormId}>
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

renderEditHelper :: forall site post tag . Entity post -> Handler site post tag (MasterForm FormData)
renderEditHelper (Entity postId post) = do
    tags <- lift $ getAutocompleteTags
    oldTags <- fmap (fmap unValue) $ lift $ runDB $ select $ from $ \tag -> do
        where_ (tag ^. pkPostTagPostField ==. val postId)
        return (tag ^. pkPostTagTagField)
    return $ renderEditForm post tags oldTags

getPKCloudBlogEditR :: forall site post tag . PostYear -> PostMonth -> PostDay -> PostLink -> Handler site post tag Html
getPKCloudBlogEditR year month day slug = do
    _ <- requireBlogUserId

    -- Lookup post.
    postM :: Maybe (Entity post) <- lift $ runDB $ getBy $ pkPostUniqueLink year month day slug
    case postM of
        Nothing ->
            lift notFound
        Just postE@(Entity _ post) -> do
            -- Check if user can edit.
            hasPermission <- lift $ pkcloudCanWrite post
            when (not hasPermission) $ 
                lift $ permissionDenied "You do not have permission to edit this post."

            -- TODO: Check if there are edits in the session. XXX
            -- Pass current edits to renderEditHelper.
            -- Display "Clear edits" button on sidebar.
            -- Display preview.
            
            -- Generate form.
            form <- renderEditHelper postE >>= lift . generateFormPost 

            -- Generate HTML.
            generateHTML year month day post form

getPKCloudBlogEditPreviewR :: forall site post tag . PostYear -> PostMonth -> PostDay -> PostLink -> Handler site post tag Html
getPKCloudBlogEditPreviewR year month day slug = do
    _ <- requireBlogUserId

    -- Lookup post.
    postM :: Maybe (Entity post) <- lift $ runDB $ getBy $ pkPostUniqueLink year month day slug
    case postM of
        Nothing ->
            lift notFound
        Just (Entity postId oldPost) -> do
            -- Check if user can edit.
            hasPermission <- lift $ pkcloudCanWrite oldPost
            when (not hasPermission) $ 
                lift $ permissionDenied "You do not have permission to edit this post."

            -- Load preview.
            aesonByteStringM <- lookupSessionBS $ pkcloudBlogPreviewEditKey postId
            case Aeson.decodeStrict =<< aesonByteStringM of
                Nothing -> do
                    lift $ pkcloudSetMessageWarning "Preview not found."
                    redirect $ PKCloudBlogEditR year month day slug
                Just ((post :: post), tags) -> do
            
            
            
                    -- TODO: Generate form. XXX

                    -- Display preview.
                    lift $ pkcloudDefaultLayout PKCloudBlogApp "Preview Post" $ do
                        pkcloudSetTitle "Preview Post"
                        [whamlet|
                            <div .container>
                                <div .row>
                                    <div .col-sm-8>
                                        ^{pkBlogRenderBlog post tags}
                        |] 

            
postPKCloudBlogEditR :: forall site post tag . PostYear -> PostMonth -> PostDay -> PostLink -> Handler site post tag Html
postPKCloudBlogEditR year month day slug = do
    _ <- requireBlogUserId

    -- Lookup post.
    postM :: Maybe (Entity post) <- lift $ runDB $ getBy $ pkPostUniqueLink year month day slug
    case postM of
        Nothing ->
            lift notFound
        Just postE@(Entity postId post) -> do
            -- Check if user can edit.
            hasPermission <- lift $ pkcloudCanWrite post
            when (not hasPermission) $ 
                lift $ permissionDenied "You do not have permission to edit this post."

            -- Check if preview was pressed or update.
            isPreview <- (== Just "preview") `fmap` lookupPostParam "submit"
            
            -- Parse form.
            ((result, formW), formE) <- renderEditHelper postE >>= lift . runFormPost
            case result of
                FormMissing -> do
                    lift $ pkcloudSetMessageDanger "Editing post failed."
                    generateHTML year month day post (formW, formE)
                FormFailure _msg -> do
                    lift $ pkcloudSetMessageDanger "Editing post failed."
                    generateHTML year month day post (formW, formE)
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
                    if isPreview then
                        previewPost postId newPost tags
                    else
                        updatePost postId newPost tags

    where
        previewPost postId post tags = do
            -- Save data into cookie.
            setSessionBS (pkcloudBlogPreviewEditKey postId) $ BSL.toStrict $ Aeson.encode (post, tags)
            redirect $ PKCloudBlogEditPreviewR year month day slug

        updatePost postId newPost tags = do
            lift $ runDB $ do

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
            lift $ pkcloudSetMessageSuccess "Successfully edited post!"

            -- Redirect.
            redirect $ PKCloudBlogEditR year month day slug

pkcloudBlogPreviewEditKey :: PKCloudBlog master post tag => Key post -> Text
pkcloudBlogPreviewEditKey postId = "pkcloud-blog-preview-edit-" <> toPathPiece postId

-- Delete post handler.
postPKCloudBlogDeleteR :: forall site post tag . PostYear -> PostMonth -> PostDay -> PostLink -> Handler site post tag Html
postPKCloudBlogDeleteR year month day slug = do
    _ <- requireBlogUserId
    -- Lookup post.
    postM :: Maybe (Entity post) <- lift $ runDB $ getBy $ pkPostUniqueLink year month day slug
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
                    redirect $ PKCloudBlogEditR year month day slug
                FormFailure _msg -> do
                    lift $ pkcloudSetMessageDanger "Deleting post failed."
                    redirect $ PKCloudBlogEditR year month day slug
                FormSuccess () -> do
                    -- Delete post.
                    lift $ runDB $ do
                        delete $ from $ \tag -> do
                            where_ (tag ^. pkPostTagPostField ==. val postId)
                        
                        deleteKey postId

                    -- Set message.
                    lift $ pkcloudSetMessageSuccess "Successfully deleted post."

                    -- Redirect to posts page.
                    redirect PKCloudBlogPostsR

