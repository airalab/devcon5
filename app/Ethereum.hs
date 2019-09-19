{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Ethereum where

import           Control.Exception             (SomeException, try)
import           Control.Monad                 (join, void, (<=<))
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.Logger          (MonadLogger, logDebug, logError,
                                                logInfo, logWarn)
import           Crypto.Ethereum               (PrivateKey, derivePubKey,
                                                importKey)
import           Crypto.Ethereum.Keyfile       (decrypt, encrypt)
import           Crypto.Random                 (getRandomBytes)
import           Data.Aeson                    (decodeFileStrict, encodeFile)
import           Data.ByteArray.HexString      (HexString)
import           Data.ByteString               (ByteString)
import           Data.Solidity.Prim.Address    (Address, fromPubKey)
import qualified Data.Text                     as T
import           Network.Ethereum.Account      (LocalKey (..), withAccount)
import           Network.Ethereum.Api.Provider (Provider, Web3Error, runWeb3')
import           Network.Ethereum.Api.Types    (DefaultBlock (..))
import           Network.Ethereum.Chain        (foundation)
import           Network.Robonomics.Liability  (Liability)
import qualified Network.Robonomics.Liability  as Liability (list, read)
import           Pipes
import           Pipes.Concurrent              (atomically, forkIO, fromInput,
                                                send, spawn, unbounded)

import           Options

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
            $logDebug "Keyfile found and loaded"
            return key

        Nothing -> do
            $logWarn "Unable to load keyfile, generating..."
            key <- liftIO $ getRandomBytes 32
            encrypted <- liftIO $ encrypt key password
            liftIO $ encodeFile keyfileName encrypted
            $logDebug $ T.pack ("Keyfile saved at " ++ show keyfileName)
            return key

    let key = importKey (rawKey :: ByteString)
    f key $ fromPubKey (derivePubKey key)

newLiabilities :: ( MonadIO m
                  , MonadLogger m
                  )
               => Provider
               -> PrivateKey
               -> Producer Liability m ()
newLiabilities provider key = fromInputM $ do
    (output, input) <- liftIO (spawn unbounded)
    liftIO . forkIO . (catchErr <=< try) $ runWeb3' provider $
        Liability.list factoryAddress (BlockWithNumber 8573788) Latest $ \_ contract -> do
            liability <- withAccount account $ Liability.read contract
            liftIO . atomically . void $ send output liability
    $logDebug "Liability listener thread launched"
    return input
  where
    fromInputM ma = fromInput =<< lift ma
    account = LocalKey key foundation  -- TODO: Export ChainID to options
    catchErr :: Either SomeException (Either Web3Error ()) -> IO ()
    catchErr = either print pure . joinEither
    joinEither :: Either a (Either b ()) -> Either (Either a b) ()
    joinEither (Right (Left e)) = Left $ Right e
    joinEither (Left e)         = Left $ Left e
    joinEither _                = Right ()
