module PKCloud.Blog.Handler.Tag (getPKCloudBlogTagR, getPKCloudBlogTagPageR) where

import Import

getPostsHelper :: Int64 -> Text -> Handler site post tag Html
getPostsHelper page tag | page < 1 = getPostsHelper 1 tag
getPostsHelper page tag = do
    -- Check if we should filter unpublished posts.
    queryFilters <- generatePostFilters

    -- Get posts.
    posts <- liftHandler $ runDB $ select $ from $ \(InnerJoin p t) -> do
        on (p ^. pkPostIdField ==. t ^. pkPostTagPostField)
        where_ (t ^. pkPostTagTagField ==. val tag &&. queryFilters p)
        limit qLimit
        offset qOffset
        orderBy [desc (p ^. pkPostDateField)]
        return p
    
    liftHandler $ pkcloudDefaultLayout PKCloudBlogApp ("#" <> tag) $ do
        pkcloudSetTitle $ toHtml tag

        -- Display previews.
        [whamlet|
            <div .container>
                <div .row>
                    <div .col-sm-8>
                        ^{displayPostPreviews posts page postsPerPage (PKCloudBlogTagR tag) (PKCloudBlogTagPageR tag)}
                    <div .col-sm-4>
        |]


    where
        postsPerPage :: Int64
        postsPerPage = 10
        qLimit = postsPerPage + 1
        qOffset = (page - 1) * postsPerPage

getPKCloudBlogTagR :: Text -> Handler site post tag Html
getPKCloudBlogTagR = getPostsHelper 1

getPKCloudBlogTagPageR :: Text -> Int64 -> Handler site post tag Html
getPKCloudBlogTagPageR = flip getPostsHelper
