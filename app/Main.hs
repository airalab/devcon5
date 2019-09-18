{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Control.Monad.Logger        (LoggingT, logError, logInfo,
                                              runStderrLoggingT)
import qualified Data.Text                   as T
import           Network.Robonomics.InfoChan (publish, subscribe)
import           Options.Applicative
import           Pipes

import           Ethereum
import           Ipfs
import           Options
import           Worker

main :: IO ()
main = run =<< execParser opts
  where
    opts = info
        (options <**> helper)
        (fullDesc <> progDesc "Robonomics Network DR-1 :: Devcon50 Worker")

run :: Options -> IO ()
run Options{..} = runStderrLoggingT $ do
    $logInfo "Devcon50 Worker init..."

    withIpfsDaemon $ \ipfsApi -> do
        $logInfo $ T.pack ("IPFS daemon launched at " ++ ipfsApi)

        withEthereumAccount $ \account address -> do
            $logInfo $ T.pack ("Using Ethereum account " ++ show address)

            let lighthouse = lighthouseOf optionsBase
            $logInfo $ T.pack ("Launch worker for " ++ lighthouse)

            runEffect $
                subscribe ipfsApi lighthouse
                >-> devcon50Worker account
                >-> publish ipfsApi lighthouse
