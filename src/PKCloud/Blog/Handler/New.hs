module PKCloud.Blog.Handler.New (getPKCloudBlogNewR, postPKCloudBlogNewR) where

import Import

import qualified Data.List as List
import qualified Data.Text as Text
import Text.Julius (rawJS)

data FormData = FormData {
      _formDataTitle :: PostTitle
    , _formDataSlug :: PostLink
    , _formDataContent :: Textarea
    , _formDataPublished :: PostPublished
    }

generateHTML :: forall site post . (MasterWidget site, Enctype) -> Handler site post Html
generateHTML (formW, formEnc) = lift $ pkcloudDefaultLayout $ do
    pkcloudSetTitle "New post"
    [whamlet|
        <div .container>
            <div .row>
                <div .col-sm-12>
                    <form role=form method=post action="@{toMasterRoute PKCloudBlogNewR}" enctype=#{formEnc}>
                        ^{formW}
                        <div .form-group .optional .pull-right>
                            <button name="preview" .btn .btn-default>
                                Preview
                            <button type="submit" name="create" .btn .btn-primary>
                                Create
                        <div .clearfix>
    |]

renderNewForm :: MasterForm FormData
renderNewForm markup = do
    titleId <- newFormIdent
    slugId <- newFormIdent
    (res, widget') <- renderBootstrap3 BootstrapBasicForm (FormData
        <$> areq textField (withId titleId titleSettings) Nothing
        <*> areq textField (withId slugId slugSettings) Nothing
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

        -- https://gist.github.com/carymrobbins/590515bb8dfb48573527
        -- bootstrapCheckBoxField :: (ToMarkup a, RenderMessage (HandlerSite m) FormMessage, Monad m) => a -> Field m Bool
        bootstrapCheckBoxField label = checkBoxField
            { fieldView = \theId name attrs val _ -> [whamlet|
                $newline never
                <div .checkbox style="margin: 0px;">
                    <label>
                        <input id=#{theId} *{attrs} type="checkbox" name=#{name} value=yes :showVal id val:checked> #{label}
                |]
            }
          where
            showVal = either $ const False
            -- style="margin-top: -20px; margin-bottom: 0">

getPKCloudBlogNewR :: Handler site post Html
getPKCloudBlogNewR = do
    -- Check if user can create posts.
    _ <- requireBlogUserId

    -- Generate form widget.
    form <- lift $ generateFormPost renderNewForm
    
    -- Generate html.
    generateHTML form

requireBlogUserId :: forall site post . Handler site post (AuthId site)
requireBlogUserId = do
    userId :: AuthId site <- lift requireAuthId

    app <- getYesod
    appEnabled <- lift $ pkcloudAppEnabled app userId
    when (not appEnabled) $
        permissionDenied "You do not have permission to create blog posts. Try enabling the PKCloud blog app."

    return userId

postPKCloudBlogNewR :: forall site post . Handler site post Html
postPKCloudBlogNewR = do
    userId <- requireBlogUserId

    -- Parse POST.
    ((result, formW), formE) <- lift $ runFormPost renderNewForm
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
            let preview = toPreview content
            now <- getCurrentTime
            let post :: post = pkPost userId slug now published title content preview Nothing

            -- Check if user can create posts. 
            hasPermission <- lift $ pkcloudCanCreate post
            when (not hasPermission) $ 
                lift $ permissionDenied "You do not have permission to do that."

            -- Insert post.
            lift $ runDB' $ insert_ post

            -- Set message.
            lift $ pkcloudSetMessageSuccess "Successfully created post!"

            -- Redirect to post's edit page. 
            redirect $ PKCloudBlogEditR slug

    where
        -- Grab the first paragraph as a preview. 
        toPreview orig = 
            -- Split into lines.
            let lines' = Text.lines orig in
            
            -- Pull out any leading empty lines. 
            let lines = List.dropWhile isEmpty lines' in

            -- Grab the first three paragraphs.
            let preview = toPreviewHelper 3 lines in

            -- Join preview lines. 
            Text.unlines preview

        toPreviewHelper :: Int -> [Text] -> [Text]
        toPreviewHelper 1 lines = List.takeWhile (not . isEmpty) lines
        toPreviewHelper c lines = 
            let (para, rest') = List.span (not . isEmpty) lines in
            let (spaces, rest) = List.span isEmpty rest' in
            para ++ spaces ++ toPreviewHelper (c - 1) rest

        isEmpty t = "" == Text.strip t


