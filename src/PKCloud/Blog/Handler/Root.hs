module PKCloud.Blog.Handler.Root where

import Import

getPKCloudBlogRootR :: Handler site post tag Html
getPKCloudBlogRootR = redirect PKCloudBlogPostsR
