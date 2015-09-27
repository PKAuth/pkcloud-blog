module PKCloud.Blog.Handler.Root where

import Import

getRootR :: Handler Html
getRootR = redirect PostsR
