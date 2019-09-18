module Ipfs where

import           Control.Monad.IO.Class (MonadIO (..))

-- | IPFS daemon launcher & observer
withIpfsDaemon :: MonadIO m => (String -> m a) -> m a
withIpfsDaemon f =
    -- TODO: Checking remote nodes liveness
    f "/ip4/127.0.0.1/tcp/5001"
