{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Robot where

import           Control.Monad                (forever, when)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.Logger         (MonadLogger, logDebug, logInfo)
import           Crypto.Ethereum              (PrivateKey)
import           Data.Solidity.Prim           (Address)
import qualified Data.Text                    as T
import           Network.Robonomics.InfoChan  (Msg (..))
import           Network.Robonomics.Liability (Liability (..))
import           Network.Robonomics.Message   (Demand (..), Offer (..))
import           Pipes

import           Options

-- | Trader thread;
--
-- Getting demands, filter and handle it, provide supply responses.
devcon50Trader :: (MonadLogger m, MonadIO m)
               => BaseName
               -> Address
               -> PrivateKey
               -> Pipe Msg Msg m ()
devcon50Trader base owner account = forever $ do
    incoming <- await
    case incoming of
        MkDemand demand -> do
            when (demandModel demand == modelOf base) $ do
                lift $ $logInfo "Get demand to building BASE"
        _ -> return ()

-- | Worker thread;
--
-- Getting liabilities, filter it, do work and report.
devcon50Worker :: (MonadLogger m, MonadIO m)
               => BaseName
               -> Address
               -> PrivateKey
               -> Pipe Liability Msg m ()
devcon50Worker base owner account = forever $ do
    Liability{..} <- await
    lift $ $logInfo $ T.pack ("Incoming liability for " ++ show liabilityPromisor)
