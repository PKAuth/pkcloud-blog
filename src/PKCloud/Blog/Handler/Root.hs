module PKCloud.Blog.Handler.Root where

import Import

getRootR :: Handler site Html
getRootR = redirect PostsR
