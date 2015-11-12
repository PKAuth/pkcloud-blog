module PKCloud.Blog.Handler.New (getPKCloudBlogNewR, postPKCloudBlogNewR) where

import Import

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
renderNewForm = renderBootstrap3 BootstrapBasicForm $ FormData
    <$> areq textField titleSettings Nothing
    <*> areq textField slugSettings Nothing

    where
        titleSettings = withAutofocus $ withPlaceholder "Title" $ 
            bfs ("Title" :: Text)

        slugSettings = withPlaceholder "Permalink" $ 
            bfs ("Permalink" :: Text)

getPKCloudBlogNewR :: Handler site post edit Html
getPKCloudBlogNewR = do
    -- TODO: Check if user can create posts.
    -- Generate form widget.
    form <- lift $ generateFormPost renderNewForm
    
    -- Generate html.
    generateHTML form

postPKCloudBlogNewR :: Handler site post edit Html
postPKCloudBlogNewR = undefined
