{-# LANGUAGE
OverloadedStrings
  #-}


module Web.Spock.Lucid
(
  lucid,
)
where


import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Web.Spock
import Lucid.Base


-- | Render HTML and send as response body. Content-type will be @text/html@.
lucid :: MonadIO m => HtmlT m a -> ActionCtxT ctx m a
lucid x = do
  setHeader "Content-Type" "text/html; charset=utf-8"
  lazyBytes =<< lift (renderBST x)
{-# INLINE lucid #-}
