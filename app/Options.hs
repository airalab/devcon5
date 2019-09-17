{-# LANGUAGE OverloadedStrings #-}
module Options where

import           Data.Solidity.Prim            (Address)
import           Network.Ethereum.Api.Provider (Provider (..))
import           Options.Applicative

data BaseName = BaseRed | BaseBlue | BaseGreen
    deriving (Eq, Ord, Show)

lighthouseOf :: BaseName -> String
lighthouseOf BaseRed   = "devcon50-red.lighthouse.5.robonomics.eth"
lighthouseOf BaseBlue  = "devcon50-blue.lighthouse.5.robonomics.eth"
lighthouseOf BaseGreen = "devcon50-grin.lighthouse.5.robonomics.eth"

infura :: Provider
infura = HttpProvider "https://mainnet.infura.io/v3/1ba07380f3e740148f89852159695c73"

data Options = Options
    { optionsProvider :: !Provider
    , optionsBase     :: !BaseName
    } deriving (Eq, Show)

options :: Parser Options
options = Options
    <$> option (HttpProvider <$> str) (long "web3" <> value infura <> metavar "URI" <> help "Ethereum node endpoint [DEFAULT: Infura mainnet]")
    <*> (   flag' BaseRed (long "red" <> help "Launch RED Devcon50 base worker")
        <|> flag' BaseBlue (long "blue" <> help "Launch BLUE Devcon50 base worker")
        <|> flag' BaseGreen (long "green" <> help "Launch GREEN Devcon50 base worker")
        )
