module PKCloud.Blog.Handler.New (getPKCloudBlogNewR, postPKCloudBlogNewR) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Char as Char
import qualified Data.Text as Text

import Import

data FormData = FormData {
      _formDataTitle :: PostTitle
    , _formDataSlug :: PostLink
    , _formDataContent :: Textarea
    , _formDataTags :: Maybe [Text]
    , _formDataPublished :: PostPublished
    }

generateHTML :: forall site post tag . (MasterWidget site, Enctype) -> Maybe (post, [Text]) -> Handler site post tag Html
generateHTML (formW, formEnc) previewM = lift $ pkcloudDefaultLayout PKCloudBlogApp "New post" $ do
    pkcloudSetTitle "New post"
    [whamlet|
        <div .container>
            <div .row>
                <div .col-sm-12>
                    <form role=form method=post action="@{toMasterRoute PKCloudBlogNewR}" enctype=#{formEnc}>
                        ^{formW}
                        <div .form-group .optional .pull-right>
                            <button type="submit" name="submit" value="preview" .btn .btn-default>
                                Preview
                            <button type="submit" name="submit" value="create" .btn .btn-primary>
                                Create
                        <div .clearfix>
                    ^{previewW}
    |]

    where
        previewW = case previewM of
            Nothing -> mempty
            Just (post, tags) -> [whamlet|
                <h1>
                    Preview
                        ^{pkBlogRenderBlog post tags}
            |]

renderNewForm :: forall site post tag . (PKCloudBlog site post tag, RenderMessage site FormMessage) => [Text] -> Maybe (post, [Text]) -> SiteForm site FormData
renderNewForm tags previewM markup = do
    titleId <- newFormIdent
    slugId <- newFormIdent
    datePieces <- splitDate <$> getCurrentTime
    (res, widget') <- renderBootstrap3 BootstrapBasicForm (FormData
        <$> areq textField (withId titleId titleSettings) defaultTitle 
        <*> areq (checkM (checkSlug datePieces) textField) (withId slugId slugSettings) Nothing
        <*> areq textareaField contentSettings defaultContent
        <*> aopt (tagField tags) tagSettings defaultTags
        <*> areq (bootstrapCheckBoxField ("Publish" :: Text)) publishSettings defaultPublish
--        <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)
      ) markup
    let widget = do
            toWidget [julius|
                (function() {
                    // Derive slug from title.
                    var cachedValue = null;
                    var updateSlug = function() {
                        var value = $(this).val()
                        if (value != cachedValue) {
                            cachedValue = value;
                            $(#{identToJavascript slugId}).val( getSlug( value));
                        }
                    };
                    $(#{identToJavascript titleId}).on('change keydown paste input mouseup', updateSlug);
                    updateSlug.call( $(#{identToJavascript titleId}));
                })();
            |]
            widget'
    return (res, widget)

    where
        (defaultTitle, defaultContent, defaultTags, defaultPublish) = case previewM of
            Nothing -> (Nothing, Nothing, Nothing, Just True)
            Just (post, tags) -> (Just (pkPostTitle post), Just (Textarea $ pkPostContent post), Just (Just tags), Just (pkPostPublished post) )
        tagSettings = withPlaceholder "Tags" $
            bfs ("Tags" :: Text)

        checkSlug :: forall site post tag . (PKCloudBlog site post tag) => (Int, Int, Int) -> PostLink -> HandlerT site IO (Either Text PostLink)
        checkSlug (year, month, day) slug = do
            -- Check that slug isn't used.
            postM :: Maybe (Entity post) <- runDB $ getBy $ pkPostUniqueLink year month day slug
            case postM of
                Nothing ->
                    -- Check that slug only is lowercase and dash characters.
                    if Text.all (\c -> Char.isDigit c || Char.isLower c || c == '-') slug then
                        return $ Right slug
                    else
                        return $ Left "Permalinks can only consist of lowercase characters and dashes."
                Just _ ->
                    return $ Left "This permalink is already taken."

        titleSettings = withAutofocus $ withPlaceholder "Title" $ 
            bfs ("Title" :: Text)

        slugSettings = readonly $ withPlaceholder "Permalink" $ 
            bfs ("Permalink" :: Text)

        contentSettings = withAttr ("rows","10") $ withPlaceholder "Content" $ 
            bfs ("Content" :: Text)

        publishSettings = "Publish"

        withId i setting = setting {fsId = Just i}

        withAttr a setting = 
            let oldS = fsAttrs setting in
            setting {fsAttrs = a:oldS}

        readonly setting = 
            let attrs = fsAttrs setting in
            setting {fsAttrs = ("readonly","readonly"):attrs}

getPendingPreview :: forall site post tag . Handler site post tag (Maybe (post, [Text]))
getPendingPreview = do
    -- Get session information for Preview 
    aesonByteStringM <- lookupSessionBS pkcloudBlogPreviewNewKey
    return $ Aeson.decodeStrict =<< aesonByteStringM
 
getPKCloudBlogNewR :: Handler site post tag Html
getPKCloudBlogNewR = do
    -- Check if user can create posts.
    _ <- requireBlogUserId

    -- Get pending preview.
    previewM <- getPendingPreview

    -- Generate form widget.
    tags <- lift getAutocompleteTags
    form <- lift $ generateFormPost $ renderNewForm tags previewM

    -- Generate html.
    generateHTML form previewM 

postPKCloudBlogNewR :: forall site post tag . Handler site post tag Html
postPKCloudBlogNewR = do
    userId <- requireBlogUserId

    -- Get pending preview.
    previewM <- getPendingPreview

    -- Parse POST.
    tags <- lift getAutocompleteTags
    ((result, formW), formE) <- lift $ runFormPost $ renderNewForm tags previewM
    case result of
        FormMissing -> do
            lift $ pkcloudSetMessageDanger "Creating post failed."
            generateHTML (formW, formE) previewM
        FormFailure _msg -> do
            lift $ pkcloudSetMessageDanger "Creating post failed."
            generateHTML (formW, formE) previewM
        FormSuccess (FormData title slug content' tagsM published) -> do

            -- Create post.
            let content = unTextarea content'
            let preview = makeBlogPreview content
            now <- getCurrentTime
            let (year, month, day) = splitDate now
            let post :: post = pkPost userId slug now published title content preview Nothing year month day
            let tags = maybe [] id tagsM

            -- Check if user can create posts. 
            hasPermission <- lift $ pkcloudCanCreate post
            when (not hasPermission) $ 
                lift $ permissionDenied "You do not have permission to do that."

            -- Check if preview was pressed or create.
            res <- lookupPostParam "submit"
            case res of
                Just "preview" -> do
                    previewPost post tags 
                _ -> do 
                    createPost formW formE tags post slug year month day previewM
    where
        createPost formW formE tags post slug year month day previewM = do
            -- Insert post.
            postM <- lift $ runDB $ insertUnique post
            case postM of
                Nothing -> do
                    -- Set message.
                    lift $ pkcloudSetMessageDanger "Another post already exists with the same permalink."
                    generateHTML (formW, formE) previewM
                Just postId -> do
                    -- Insert tags.
                    lift $ runDB $ mapM_ (insert_ . pkPostTag postId) tags

                    -- Delete any saved previews.
                    deleteSession pkcloudBlogPreviewNewKey

                    -- Set message.
                    lift $ pkcloudSetMessageSuccess "Successfully created post!"

                    -- Redirect to post's edit page. 
                    redirect $ PKCloudBlogEditR year (Int2 month) (Int2 day) slug

        previewPost post tags = do
           -- Save post data into cookie
           -- Aeson Encode the post and tags
            setSessionBS pkcloudBlogPreviewNewKey $ BSL.toStrict $ Aeson.encode (post, tags)
            redirect $ PKCloudBlogNewR 

pkcloudBlogPreviewNewKey :: Text
pkcloudBlogPreviewNewKey = "pkcloud-blog-preview-new"

