{-# LANGUAGE OverloadedStrings #-}
module Options where

import           Data.Base58String             (Base58String)
import           Data.Base58String.Bitcoin     (b58String)
import           Data.ByteString               (ByteString)
import           Data.Solidity.Prim            (Address)
import           Network.Ethereum.Api.Provider (Provider (..))
import           Options.Applicative

data BaseName = BaseRed | BaseBlue | BaseGreen
    deriving (Eq, Ord, Show)

modelOf :: BaseName -> Base58String
modelOf BaseRed   = b58String "QmZQKV8E3hjyxnEQGq1d4XCmeUDfzkAhryoJG3pcBkoUE6"
modelOf BaseBlue  = b58String "QmRX4ekTAWnnKD1d5qfFkKk3HoArC1itF2XcEMAkh56Lva"
modelOf BaseGreen = b58String "QmQU4tWfKvTQza7ydMR3zZFe9C8NicpLFkCQazK72TKx3m"

infura :: Provider
infura = HttpProvider "https://mainnet.infura.io/v3/1ba07380f3e740148f89852159695c73"

lighthouse :: String
lighthouse = "devcon50.lighthouse.5.robonomics.eth"

keyfileName :: FilePath
keyfileName = "worker-keyfile.json"

password :: ByteString
password = "devcon50"

factoryAddress :: Address
factoryAddress = "0x7e384AD1FE06747594a6102EE5b377b273DC1225"

data Options = Options
    { optionsProvider :: !Provider
    , optionsBase     :: !BaseName
    , optionsOwner    :: !Address
    } deriving (Eq, Show)

options :: Parser Options
options = Options
    <$> option (HttpProvider <$> str) (long "web3" <> value infura <> metavar "URI" <> help "Ethereum node endpoint [DEFAULT: Infura mainnet]")
    <*> (   flag' BaseRed (long "construction" <> help "Launch Сonstruction Devcon50 base worker")
        <|> flag' BaseBlue (long "life" <> help "Launch Life Devcon50 base worker")
        <|> flag' BaseGreen (long "brewery" <> help "Launch Brewery Devcon50 base worker")
        )
    <*> argument str (metavar "ADDRESS" <> help "Ethereum address of robot owner")
