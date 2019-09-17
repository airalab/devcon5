module Ipfs where

import           Control.Monad.IO.Class (MonadIO (..))

-- | IPFS daemon launcher & observer
withIpfsDaemon :: MonadIO m => (String -> m a) -> m a
withIpfsDaemon f = do
    undefined
