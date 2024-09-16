module LambdaBuffers.Frontend.Cli.Env (lbcVar, lbgVar, getLbcFromEnvironment, getLbgFromEnvironment) where

import LambdaBuffers.Logger (logError)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)

lbcVar :: String
lbcVar = "LB_COMPILER"

lbgVar :: String
lbgVar = "LB_CODEGEN"

getLbcFromEnvironment :: IO String
getLbcFromEnvironment = do
  mayLbc <- lookupEnv lbcVar
  maybe
    ( do
        logError "" $ lbcVar <> " environment variable is missing"
        exitFailure
    )
    return
    mayLbc

getLbgFromEnvironment :: IO String
getLbgFromEnvironment = do
  mayLbg <- lookupEnv lbgVar
  maybe
    ( do
        logError "" $ lbgVar <> " environment variable is missing"
        exitFailure
    )
    return
    mayLbg
