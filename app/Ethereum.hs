{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Ethereum where

import           Control.Exception          (SomeException, try)
import           Control.Monad              (join)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Logger       (MonadLogger, logError, logInfo)
import           Crypto.Ethereum            (PrivateKey, derivePubKey,
                                             importKey)
import           Crypto.Ethereum.Keyfile    (decrypt, encrypt)
import           Crypto.Random              (getRandomBytes)
import           Data.Aeson                 (decodeFileStrict, encodeFile)
import           Data.ByteArray.HexString   (HexString)
import           Data.ByteString            (ByteString)
import           Data.Solidity.Prim.Address (Address, fromPubKey)
import qualified Data.Text                  as T

keyfileName :: FilePath
keyfileName = "worker-keyfile.json"

password :: ByteString
password = "devcon50"

withEthereumAccount :: ( MonadIO m
                       , MonadLogger m
                       )
                    => (PrivateKey -> Address -> m a)
                    -> m a
withEthereumAccount f = do
    let joinErr :: Either SomeException (Maybe a) -> Maybe a
        joinErr = join . either (const Nothing) Just
    keyfile <- joinErr <$> liftIO (try $ decodeFileStrict keyfileName)

    rawKey <- case flip decrypt password =<< keyfile of
        Just key -> do
            $logInfo "Keyfile found and loaded"
            return key

        Nothing -> do
            $logInfo "Unable to load keyfile, generating..."
            key <- liftIO $ getRandomBytes 32
            encrypted <- liftIO $ encrypt key password
            liftIO $ encodeFile keyfileName encrypted
            $logInfo $ T.pack ("Keyfile saved at " ++ show keyfileName)
            return key

    let key = importKey (rawKey :: ByteString)
    f key $ fromPubKey (derivePubKey key)
