{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, ConstraintKinds #-}

module Import (
      module Export
    , Handler
    ) where

import PKCloud.Import as Export

import PKCloud.Blog.Core as Export
import PKCloud.Blog.Routes as Export

-- import Control.Monad.IO.Class
-- import Control.Monad.Trans.Reader
-- import Database.Persist.Class
-- import Database.Persist.Sql (SqlBackend)



type Handler master a = (PKCloudBlog master) => HandlerT PKCloudBlogApp (HandlerT master IO) a
-- type Handler a = (MonadHandler m, MonadBaseControl IO m) => HandlerT PKCloudBlogApp (HandlerT master m) a


-- class Monad m => GeneralPersist site m | m -> site where
--     type GeneralPersistBackend site
--     runDB' :: ReaderT (GeneralPersistBackend site) m a -> m a
-- 
-- class (GeneralPersistBackend site ~ SqlBackend, MonadIO m, GeneralPersist site m) => GeneralPersistSql site m
-- 
-- class (PersistEntity e, SqlBackend ~ PersistEntityBackend e) => SubEntity e
