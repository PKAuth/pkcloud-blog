module PKCloud.Blog.Handler.New (getPKCloudBlogNewR, postPKCloudBlogNewR) where

import Import

import Text.Julius (rawJS)

data FormData = FormData {
      _formDataTitle :: PostTitle
    , _formDataSlug :: PostLink
    }

generateHTML :: forall site post edit . (MasterWidget site, Enctype) -> Handler site post edit Html
generateHTML (formW, formEnc) = lift $ pkcloudDefaultLayout [whamlet|
        <form role=form method=post action="@{toMasterRoute PKCloudBlogNewR}" enctype=#{formEnc}>
            ^{formW}
    |]

renderNewForm :: MasterForm FormData
renderNewForm markup = do
    titleId <- newFormIdent
    slugId <- newFormIdent
    (res, widget') <- renderBootstrap3 BootstrapBasicForm (FormData
        <$> areq textField (withId titleId titleSettings) Nothing
        <*> areq textField (withId slugId slugSettings) Nothing
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

        withId i setting = setting {fsId = Just i}

        disable setting = 
            let attrs = fsAttrs setting in
            setting {fsAttrs = ("disabled","disabled"):attrs}

getPKCloudBlogNewR :: Handler site post edit Html
getPKCloudBlogNewR = do
    -- TODO: Check if user can create posts.
    -- Generate form widget.
    form <- lift $ generateFormPost renderNewForm
    
    -- Generate html.
    generateHTML form

postPKCloudBlogNewR :: Handler site post edit Html
postPKCloudBlogNewR = undefined
