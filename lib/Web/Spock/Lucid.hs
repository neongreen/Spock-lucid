{-# LANGUAGE
CPP,
OverloadedStrings,
RankNTypes
  #-}


module Web.Spock.Lucid
(
  lucid,
  lucidIO,
  lucidT,
)
where


import Data.Functor.Identity
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Web.Spock
import Lucid.Base
import Blaze.ByteString.Builder

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mempty)
#endif


-- | Render HTML and send as response body. Content-type will be @text/html@.
lucid :: MonadIO m => Html a -> ActionCtxT cxt m b
lucid x = do
  setHeader "Content-Type" "text/html; charset=utf-8"
  let Identity (render, _) = runHtmlT x
  lazyBytes (toLazyByteString (render mempty))
{-# INLINE lucid #-}

-- | Like 'lucid', but for @HtmlT IO@.
lucidIO :: MonadIO m => HtmlT IO a -> ActionCtxT cxt m b
lucidIO x = do
  setHeader "Content-Type" "text/html; charset=utf-8"
  (render, _) <- liftIO (runHtmlT x)
  lazyBytes (toLazyByteString (render mempty))
{-# INLINE lucidIO #-}

-- | Like 'lucid', but for arbitrary monads. Might require some additional
-- boilerplate.
lucidT :: MonadIO m => HtmlT m a -> ActionCtxT cxt m b
lucidT x = do
  setHeader "Content-Type" "text/html; charset=utf-8"
  (render, _) <- lift (runHtmlT x)
  lazyBytes (toLazyByteString (render mempty))
{-# INLINE lucidT #-}
