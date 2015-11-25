module PKCloud.Blog.Handler.New (getPKCloudBlogNewR, postPKCloudBlogNewR) where

import Control.Monad.Trans.Reader
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import qualified Data.Text as Text
import Text.Julius (rawJS)

import Import

data FormData = FormData {
      _formDataTitle :: PostTitle
    , _formDataSlug :: PostLink
    , _formDataContent :: Textarea
--    , _formDataTags :: [Text]
    , _formDataPublished :: PostPublished
    }

generateHTML :: forall site post tag . (MasterWidget site, Enctype) -> Handler site post tag Html
generateHTML (formW, formEnc) = lift $ pkcloudDefaultLayout PKCloudBlogApp $ do
    pkcloudSetTitle "New post"
    [whamlet|
        <div .container>
            <div .row>
                <div .col-sm-12>
                    <form role=form method=post action="@{toMasterRoute PKCloudBlogNewR}" enctype=#{formEnc}>
                        ^{formW}
                        <div .form-group .optional .pull-right>
                            <a href="#" .btn .btn-default>
                                Preview
                            <button type="submit" name="create" .btn .btn-primary>
                                Create
                        <div .clearfix>
    |]

renderNewForm :: Aeson.Value -> MasterForm FormData
renderNewForm tags markup = do
    titleId <- newFormIdent
    slugId <- newFormIdent
    (res, widget') <- renderBootstrap3 BootstrapBasicForm (FormData
        <$> areq textField (withId titleId titleSettings) Nothing
        <*> areq (checkM checkSlug textField) (withId slugId slugSettings) Nothing
        <*> areq textareaField contentSettings Nothing
        <*> areq (bootstrapCheckBoxField ("Publish" :: Text)) publishSettings (Just True)
--        <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)
      ) markup
    let widget = do
            toWidget [julius|
                (function() {
                    var cachedValue = null;
                    $('##{rawJS titleId}').on('change keydown paste input mouseup', function() {
                        var value = $(this).val()
                        if (value != cachedValue) {
                            cachedValue = value;
                            $('##{rawJS slugId}').val( getSlug( value));
                        }
                    });
                })();
            |]
            widget'
    return (res, widget)

    where
        checkSlug :: forall site post tag . (PKCloudBlog site post tag) => PostLink -> HandlerT site IO (Either Text PostLink)
        checkSlug slug = do
            -- Check that slug isn't used.
            postM :: Maybe (Entity post) <- runDB' $ getBy $ pkPostUniqueLink slug
            case postM of
                Nothing ->
                    -- Checek that slug only is lowercase and dash characters.
                    if Text.all (\c -> Char.isLower c || c == '-') slug then
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

getAutocompleteTags :: forall site post tag . Handler site post tag Aeson.Value
getAutocompleteTags = do
    -- Get distinct tags from DB.
    -- tags <- runDB $ select $ distinct $ from $ \tag -> do
    --     return (tag ^. pkPostTagTagField)
    tags :: [Value Text] <- runDB $ select $ distinct $ from $ \(tag :: SqlExpr (Entity tag)) -> do
        return (tag ^. pkPostTagTagField)
    return $ Aeson.toJSON $ fmap unValue tags

    where
        -- runDB :: forall m a . (GeneralPersistBackend site ~ SqlBackend) => ReaderT (GeneralPersistBackend site) m a -> m a
        runDB :: ReaderT (GeneralPersistBackend site) m a -> m a
        runDB = runDB'

getPKCloudBlogNewR :: Handler site post tag Html
getPKCloudBlogNewR = do
    -- Check if user can create posts.
    _ <- requireBlogUserId

    -- Generate form widget.
    tags <- getAutocompleteTags
    form <- lift $ generateFormPost $ renderNewForm tags
    
    -- Generate html.
    generateHTML form

postPKCloudBlogNewR :: forall site post tag . Handler site post tag Html
postPKCloudBlogNewR = do
    userId <- requireBlogUserId

    -- Parse POST.
    tags <- getAutocompleteTags
    ((result, formW), formE) <- lift $ runFormPost $ renderNewForm tags
    case result of
        FormMissing -> do
            lift $ pkcloudSetMessageDanger "Creating post failed."
            generateHTML (formW, formE)
        FormFailure _msg -> do
            lift $ pkcloudSetMessageDanger "Creating post failed."
            generateHTML (formW, formE)
        FormSuccess (FormData title slug content' published) -> do
            -- Create post.
            let content = unTextarea content'
            let preview = makeBlogPreview content
            now <- getCurrentTime
            let post :: post = pkPost userId slug now published title content preview Nothing

            -- Check if user can create posts. 
            hasPermission <- lift $ pkcloudCanCreate post
            when (not hasPermission) $ 
                lift $ permissionDenied "You do not have permission to do that."

            -- Insert post.
            postM <- lift $ runDB' $ insertUnique post
            case postM of
                Nothing -> do
                    -- Set message.
                    lift $ pkcloudSetMessageDanger "Another post already exists with the same permalink."
                    generateHTML (formW, formE)
                Just _ -> do
                    -- Set message.
                    lift $ pkcloudSetMessageSuccess "Successfully created post!"

                    -- Redirect to post's edit page. 
                    redirect $ PKCloudBlogEditR slug

