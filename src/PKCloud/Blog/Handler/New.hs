module PKCloud.Blog.Handler.New (getPKCloudBlogNewR, postPKCloudBlogNewR) where

import Import

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

        slugSettings = disable $ withPlaceholder "Permalink" $ 
            bfs ("Permalink" :: Text)

        contentSettings = withPlaceholder "Content" $ 
            bfs ("Content" :: Text)

        publishSettings = "Publish"

        withId i setting = setting {fsId = Just i}

        disable setting = 
            let attrs = fsAttrs setting in
            setting {fsAttrs = ("disabled","disabled"):attrs}

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
    -- TODO: Check if user can create posts. XXX
    -- Generate form widget.
    form <- lift $ generateFormPost renderNewForm
    
    -- Generate html.
    generateHTML form

postPKCloudBlogNewR :: Handler site post Html
postPKCloudBlogNewR = undefined
    -- TODO: Check if user can create posts. XXX
