{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Ipfs where

import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Monad          (forever)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Logger   (MonadLogger, logDebug, logError,
                                         logInfo)
import qualified Data.Text              as T
import           System.Exit            (ExitCode (..), exitFailure)
import           System.Process         (proc, readCreateProcessWithExitCode)

airalabIpfsApi :: String
airalabIpfsApi = "/dns4/ipfsapi.devcon50.aira.life/tcp/5001"

-- | IPFS daemon launcher & observer
withIpfsDaemon :: (MonadLogger m, MonadIO m)
               => (String -> m a)
               -> m a
withIpfsDaemon f = do
    mbapi <- takeFirstM checkApi apis
    case mbapi of
        Just api -> do
            $logInfo $ T.pack ("Using IPFS api at " ++ api)
            liftIO $ forkIO $ forever $ do
                swarmConnect api
                threadDelay 20000000
            f api
        Nothing -> do
            $logError "IPFS initialisation error"
            liftIO exitFailure
  where
    apis = [ "/ip4/127.0.0.1/tcp/5001", airalabIpfsApi ]
    checkApi api = do
        let ipfs = proc "ipfs" ["--api", api, "swarm", "peers"]
        (code, _, _) <- liftIO $ readCreateProcessWithExitCode ipfs ""
        case code of
            ExitSuccess   -> do
                $logDebug $ T.pack ("IPFS: success connect to " ++ api)
                return True
            ExitFailure _ -> do
                $logDebug $ T.pack ("IPFS: unable connect to " ++ api)
                return False
    swarmConnect api = do
        let ipfs = proc "ipfs" ["--api", api, "swarm", "connect", "/dnsaddr/bootstrap.aira.life"]
        readCreateProcessWithExitCode ipfs ""


takeFirstM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
takeFirstM _ [] = return Nothing
takeFirstM f (x : xs) = do
    success <- f x
    if success then return (Just x)
               else takeFirstM f xs
