{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Robot where

import           Control.Monad                 (forever, when)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.Logger          (MonadLogger, logDebug, logError,
                                                logInfo)
import           Crypto.Ethereum               (PrivateKey)
import           Data.Base58String             (Base58String)
import           Data.Base58String.Bitcoin     (b58String)
import           Data.ByteArray.HexString      (fromBytes, toText)
import           Data.Solidity.Prim            (Address)
import qualified Data.Text                     as T
import           Network.Ethereum.Api.Provider (Provider)
import           Network.Robonomics.InfoChan   (Msg (..))
import           Network.Robonomics.Liability  (Liability (..))
import           Network.Robonomics.Message    (Demand (..), Offer (..),
                                                Report (..), RobonomicsMsg (..))
import           Pipes

import           Ethereum
import           Options

-- | Trader thread;
--
-- Getting demands, filter and handle it, provide supply responses.
devcon50Trader :: (MonadLogger m, MonadIO m)
               => BaseName
               -> Base58String
               -> Provider
               -> KeyPair
               -> Pipe Msg Msg m ()
devcon50Trader base objective provider (key, address) = forever $ do
    incoming <- await
    case incoming of
        MkDemand demand -> do
            lift $ $logDebug $ T.pack $ "Received DEMAND " ++ show demand
            when (demandModel demand == modelOf base && demandObjective demand == objective) $ do
                mboffer <- lift $ do
                    $logInfo "Received demand from owner, reply offer"
                    let setNonce nonce = (copyDemand demand) { offerNonce = nonce }
                    fmap setNonce <$> getNonce provider address
                case mboffer of
                    Right offer -> do
                        yield . MkOffer $ offer { offerSignature = sign key offer }
                        lift $ $logDebug $ T.pack $ "Offer sended: " ++ show offer
                    Left e ->
                        lift $ $logError $ T.pack ("Unable to make offer: " ++ show e)
        _ -> return ()
  where
    copyDemand Demand{..} =
        Offer demandModel demandObjective demandToken demandCost
              demandValidator demandLighthouse 0 demandDeadline 0 address ""

-- | Worker thread;
--
-- Getting liabilities, filter it, do work and report.
devcon50Worker :: (MonadLogger m, MonadIO m)
               => BaseName
               -> KeyPair
               -> Pipe (Address, Liability) Msg m ()
devcon50Worker base (key, address) = forever $ do
    (liabilityAddress, Liability{..}) <- await
    lift $ $logDebug $ T.pack ("Incoming liability for " ++ show liabilityPromisor)

    when (liabilityPromisor == address) $ do
        lift $ $logInfo "I've get liability, start working..."
        -- TODO: funny works BEGIN
        let result = b58String "QmWboFP8XeBtFMbNYK3Ne8Z3gKFBSR5iQzkKgeNgQz3dz4"
        -- TODO: funny works END
        lift $ $logInfo "Work finished, send report"
        let report = Report liabilityAddress result True ""
        yield $ MkReport (report { reportSignature = sign key report})
        lift $ $logDebug $ T.pack $ "Report sended: " ++ show report
