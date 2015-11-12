module PKCloud.Blog.Handler.Root where

import Import

getPKCloudBlogRootR :: Handler site post edit Html
getPKCloudBlogRootR = redirect PKCloudBlogPostsR
