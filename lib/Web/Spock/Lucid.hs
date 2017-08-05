{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module Web.Spock.Lucid
(
  lucid,
  lucidIO,
  lucidT,
)
where


import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Web.Spock
import Lucid.Base


-- | Render HTML and send as response body. Content-type will be @text/html@.
lucid :: MonadIO m => Html a -> ActionCtxT cxt m b
lucid x = do
  setHeader "Content-Type" "text/html; charset=utf-8"
  lazyBytes (renderBS x)
{-# INLINE lucid #-}

-- | Like 'lucid', but for @HtmlT IO@.
lucidIO :: MonadIO m => HtmlT IO a -> ActionCtxT cxt m b
lucidIO x = do
  setHeader "Content-Type" "text/html; charset=utf-8"
  lazyBytes =<< liftIO (renderBST x)
{-# INLINE lucidIO #-}

-- | Like 'lucid', but for arbitrary monads. Might require some additional
-- boilerplate.
lucidT :: MonadIO m => HtmlT m a -> ActionCtxT cxt m b
lucidT x = do
  setHeader "Content-Type" "text/html; charset=utf-8"
  lazyBytes =<< lift (renderBST x)
{-# INLINE lucidT #-}
