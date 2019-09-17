module Worker where

import           Control.Monad.IO.Class      (MonadIO (..))
import           Crypto.Ethereum             (PrivateKey)
import           Network.Robonomics.InfoChan (Msg)
import           Pipes

devcon50Worker :: MonadIO m => PrivateKey -> Pipe Msg Msg m ()
devcon50Worker account = do
    undefined

