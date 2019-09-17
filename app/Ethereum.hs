module Ethereum where

import           Control.Monad.IO.Class (MonadIO)
import           Crypto.Ethereum        (PrivateKey)

withEthereumAccount :: MonadIO m => (PrivateKey -> m a) -> m a
withEthereumAccount f = do
    undefined
