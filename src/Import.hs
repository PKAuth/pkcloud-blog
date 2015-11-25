{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, ConstraintKinds #-}

module Import (
      module Export
    , Handler
--     , Widget
    , MasterWidget
    , MasterForm
    , maybeBlogUserId
    , requireBlogUserId
    , makeBlogPreview
    , tagField
    , getAutocompleteTags
    , autocompleteTextField
    , identToJavascript
    ) where

import Control.Monad as Export (when)
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
import PKCloud.Import as Export

import PKCloud.Blog.Core as Export
import PKCloud.Blog.Routes as Export

-- TODO: Move to PKCloud.Import?
import Text.Blaze (Markup)
-- | Type for forms in master site.
type MasterForm a = forall site post tag . (PKCloudBlog site post tag, RenderMessage site FormMessage) => Markup -> MForm (HandlerT site IO) (FormResult a, WidgetT site IO ())



-- type Handler master post a = (ToMasterRoute PKCloudBlogApp master, PKCloudBlog master post) => HandlerT PKCloudBlogApp (HandlerT master IO) a
type Handler master post tag a = (ToMasterRoute PKCloudBlogApp master, PKCloudBlog master post tag) => HandlerT PKCloudBlogApp (HandlerT master IO) a


-- type Widget master post edit = (PKCloudBlog master post edit) => WidgetT PKCloudBlogApp (HandlerT master IO) ()
-- type MasterWidget master = forall post edit . (PKCloudBlog master post edit) => WidgetT master IO ()
type MasterWidget master = WidgetT master IO ()

maybeBlogUserId :: forall site post tag . Handler site post tag (Maybe (AuthId site))
maybeBlogUserId = do
    userM <- lift maybeAuthId
    case userM of
        Nothing ->
            return Nothing
        Just userId -> do
            app <- getYesod
            appEnabled <- lift $ pkcloudAppEnabled app userId
            if appEnabled then
                return $ Just userId
            else
                return Nothing

requireBlogUserId :: forall site post tag . Handler site post tag (AuthId site)
requireBlogUserId = do
    userId :: AuthId site <- lift requireAuthId

    app <- getYesod
    appEnabled <- lift $ pkcloudAppEnabled app userId
    when (not appEnabled) $
        permissionDenied "You do not have permission to create blog posts. Try enabling the PKCloud blog app."

    return userId

getAutocompleteTags :: forall site post tag . (PKCloudBlog site post tag) => HandlerT site IO [Text]
getAutocompleteTags = do
    -- Get distinct tags from DB.
    tags <- runDB' $ select $ distinct $ from $ \(tag :: SqlExpr (Entity tag)) -> do
        return (tag ^. pkPostTagTagField)
    return $ fmap unValue tags

-- | Grab the first three paragraphs as a preview. 
makeBlogPreview :: Text -> Text
makeBlogPreview orig = 
    -- Split into lines.
    let lines' = Text.lines orig in
    
    -- Pull out any leading empty lines. 
    let lines = List.dropWhile isEmpty lines' in

    -- Grab the first three paragraphs.
    let preview = toPreviewHelper 3 lines in

    -- Join preview lines. 
    Text.unlines preview

    where
        toPreviewHelper :: Int -> [Text] -> [Text]
        toPreviewHelper 1 lines = List.takeWhile (not . isEmpty) lines
        toPreviewHelper c lines = 
            let (para, rest') = List.span (not . isEmpty) lines in
            let (spaces, rest) = List.span isEmpty rest' in
            para ++ spaces ++ toPreviewHelper (c - 1) rest

        isEmpty t = "" == Text.strip t

-- Make sure tags are only characters, underscores, or dashed.
tagField :: forall m . (Monad m, RenderMessage (HandlerSite m) FormMessage) => [Text] -> Field m [Text]
tagField tags = check (\tags -> 
        if List.any (not . Text.all (\c -> Char.isAlphaNum c || c == '-' || c == '_')) tags then
            Left ("Tags may only contain alphanumeric characters, underscores, or dashes." :: Text)
        else
            Right tags
    ) $ autocompleteTextField tags


-- TODO: move to PKCloud?
-- | Textfield with autocompletion suggestions. Requires [bootstrap-tokenfield](http://sliptree.github.io/bootstrap-tokenfield/). 
autocompleteTextField :: forall m . (Monad m, RenderMessage (HandlerSite m) FormMessage) => [Text] -> Field m [Text]
autocompleteTextField suggestions = Field (parseHelper parser) view UrlEncoded
    where
        view :: FieldViewFunc m [Text]
        view id name attr res req = do
            toWidget [julius|
                $(#{identToJavascript id}).tokenfield({
                    autocomplete: {
                        source: #{Aeson.toJSON suggestions},
                        delay: 100
                    },
                    showAutocompleteOnFocus: false,
                    delimiter: ',',
                    createTokensOnBlur: true
                });
            |]
            let res' = fmap (Text.intercalate ", ") res
            (fieldView (textField :: Field m Text)) id name attr res' req
            
        parser = Right . fmap Text.strip . Text.split (== ',')

-- Convert an identity to an Aeson.Value, which can be embeded in javascript. 
identToJavascript :: Text -> Aeson.Value
identToJavascript = Aeson.toJSON .("#" <>)
