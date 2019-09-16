{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Control.Monad.Logger (LoggingT, logError, logInfo,
                                       runStderrLoggingT)
import           Options.Applicative

import           Options

main :: IO ()
main = run =<< execParser opts
  where
    opts = info
        (options <**> helper)
        (fullDesc <> progDesc "Robonomics Dummy Robot :: Devcon50 Worker")

run :: Options -> IO ()
run Options{..} = runStderrLoggingT $ do
    $logInfo "Devcon50 Worker launched"
    return ()
