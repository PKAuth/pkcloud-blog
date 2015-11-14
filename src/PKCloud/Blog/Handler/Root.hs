module PKCloud.Blog.Handler.Root where

import Import

getPKCloudBlogRootR :: Handler site post Html
getPKCloudBlogRootR = redirect PKCloudBlogPostsR
