{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Control.Concurrent          (forkIO)
import           Control.Concurrent.Chan     (newChan)
import           Control.Monad.Logger        (LoggingT, logDebug, logError,
                                              logInfo, runChanLoggingT,
                                              runStderrLoggingT, unChanLoggingT)
import qualified Data.Text                   as T
import           Network.Robonomics.InfoChan (publish, subscribe)
import           Options.Applicative
import           Pipes

import           Ethereum
import           Ipfs
import           Options
import           Robot

main :: IO ()
main = run =<< execParser opts
  where
    opts = info
        (options <**> helper)
        (fullDesc <> progDesc "Robonomics Network DR-1 :: Devcon50 Worker")

run :: Options -> IO ()
run Options{..} = runStderrLoggingT $ do
    $logInfo "Devcon50 Worker init..."
    $logInfo $ T.pack ("My base: " ++ show optionsBase)
    $logInfo $ T.pack ("My owner: " ++ show optionsOwner)

    withIpfsDaemon $ \ipfsApi -> do
        $logInfo $ T.pack ("IPFS connected to " ++ ipfsApi)

        withEthereumAccount $ \key address -> do
            $logInfo $ T.pack ("Ethereum address initialized: " ++ show address)

            -- Create log channel
            logChan <- liftIO newChan

            -- Spawn worker thread
            liftIO $ forkIO $ runChanLoggingT logChan $ do
                $logDebug "Worker thread launched"
                runEffect $
                    newLiabilities optionsProvider key
                    >-> devcon50Worker optionsBase optionsOwner key
                    >-> publish ipfsApi lighthouse

            -- Spawn trader thread
            liftIO $ forkIO $ runChanLoggingT logChan $ do
                $logDebug "Trader thread launched"
                runEffect $
                    subscribe ipfsApi lighthouse
                    >-> devcon50Trader optionsBase optionsOwner key
                    >-> publish ipfsApi lighthouse

            -- Logging in main thread
            unChanLoggingT logChan
