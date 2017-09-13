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
    , generatePostFilters
    , displayPostPreviews
    , makeColumns
    ) where

import Control.Monad as Export (when)
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import Database.Esqueleto (Value)
import qualified Data.List as List
import qualified Data.Text as Text
import PKCloud.Import as Export

import PKCloud.Blog.Core as Export

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
    tags <- runDB $ select $ distinct $ from $ \(tag :: SqlExpr (Entity tag)) -> do
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
    let preview' = toPreviewHelper 3 lines in

    -- Join preview lines. 
    let preview = Text.unlines preview' in

    -- Close any open code blocks.
    closeCodeBlocks preview


    where
        toPreviewHelper :: Int -> [Text] -> [Text]
        toPreviewHelper 1 lines = List.takeWhile (not . isEmpty) lines
        toPreviewHelper c lines = 
            let (para, rest') = List.span (not . isEmpty) lines in
            let (spaces, rest) = List.span isEmpty rest' in
            para ++ spaces ++ toPreviewHelper (c - 1) rest

        isEmpty t = "" == Text.strip t

        closeCodeBlocks p = 
            -- Counts of "```"
            let (_, count) = Text.foldl' (\(c, acc) char -> 
                    if char == '`' then
                        case c of
                            Just c | c >= 2 -> (Nothing, acc + 1)
                            Just c -> (Just (c + 1), acc)
                            Nothing -> (Just 1, acc)
                    else
                        (Nothing, acc)
                  ) (Nothing, 0 :: Int) p
            in
            if odd count then
                p <> "```"
            else
                p

-- Make sure tags are only characters, underscores, or dashed.
tagField :: forall m . (Monad m, RenderMessage (HandlerSite m) FormMessage) => [Text] -> Field m [Text]
tagField tags = check (\tags -> 
        if List.any (not . Text.all (\c -> Char.isLower c || Char.isDigit c || c == '-' || c == '_')) tags then
            Left ("Tags may only contain lowercase alphanumeric characters, underscores, or dashes." :: Text)
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

generatePostFilters :: Handler site post tag (SqlExpr (Entity post) -> SqlExpr (Value Bool))
generatePostFilters = do
    userM <- maybeBlogUserId
    case userM of
        Nothing ->
            -- Not logged in so filter out unpublished posts.
            return $ \p -> p ^. pkPostPublishedField ==. val True
        Just userId ->
            -- If the post is unpublished, only show if the user is the author?
            return $ \p -> p ^. pkPostPublishedField ==. val True ||. p ^. pkPostAuthorField ==. val userId

displayPostPreviews :: forall site post tag . (PKCloudBlog site post tag, ToMasterRoute PKCloudBlogApp site) => [Entity post] -> Int64 -> Int64 -> Route PKCloudBlogApp -> (Int64 -> Route PKCloudBlogApp) -> MasterWidget site
displayPostPreviews posts page postsPerPage route routePage = 
        case posts of
            [] -> 
                if page == 1 then
                    [whamlet|
                        There are no posts yet. Check back later!
                    |]
                else
                    notFound
            _ -> do
                -- Display up to 10 of their previews.
                let postsW = mconcat $ map pkBlogDisplayPreview $ List.take postsPerPage' posts

                [whamlet|
                    ^{postsW}
                    ^{navigationW page posts}
                |]

    where
        postsPerPage' :: Int
        postsPerPage' = fromInteger $ toInteger postsPerPage
        qLimit' = postsPerPage' + 1

        navigationW :: Int64 -> [a] -> WidgetT site IO ()
        navigationW page l = do
            -- Check if we should display the older button.
            let masterPostsRoute = toMasterRoute . routePage
            let older = if List.length l == qLimit' then
                    [whamlet|
                        <ul .nav .nav-pills .pull-right>
                            <li role="presentation">
                                <a href="@{masterPostsRoute (page + 1)}">
                                    Older
                    |]
                  else
                    mempty

            -- Check if we should display the newer button.
            let newer = case page of
                    2 ->
                        [whamlet|
                            <ul .nav .nav-pills>
                                <li role="presentation">
                                    <a href="@{toMasterRoute route}">
                                        Newer
                        |]
                    1 -> 
                        mempty
                    _ ->
                        [whamlet|
                            <ul .nav .nav-pills>
                                <li role="presentation">
                                    <a href="@{masterPostsRoute (page - 1)}">
                                        Newer
                        |]

            toWidget [lucius|
                .blog-title {
                    margin-bottom: 3px;
                }

                .blog-title a {
                    color: rgb(51, 51, 51);
                }

                .blog-content {
                    margin-top: 15px;
                    margin-bottom: 35px;
                }

                .unpublished span {
                    top: -.3em;
                    position: relative;
                    margin-left: 5px;
                }
            |]

            [whamlet|
                <div>
                    ^{newer}
                    ^{older}
                <div .clearfix>
            |]

makeColumns :: Maybe (WidgetT s IO ()) -> (WidgetT s IO ()) -> (WidgetT s IO ())
makeColumns Nothing w = [whamlet|
        <div .col-xs-12>
            ^{w}
    |]

makeColumns (Just sidebar) w = [whamlet|
        <div .col-xs-8>
            ^{w}
        <div .col-xs-4>
            ^{sidebar}
    |]
