{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


import Cardano.BM.Setup (setupTrace_)
import Cardano.Node.Types (MockServerConfig (..))
import ContractExample.GuessGame
import Control.Applicative

import Control.Monad.IO.Class
import DAppFlow.Wallet
import Data.Aeson
import Data.Default (Default (def))
import Data.Maybe
import Data.TCache as TC hiding (onNothing)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Yaml (decodeFileThrow)
import GHC.Generics (Generic)
import qualified Ledger.Ada as Ada
import Playground.Types (FunctionSchema)
import Plutus.Contract.Types (Promise (..))
import Plutus.Monitoring.Util (PrettyObject (..), convertLog)
import Plutus.PAB.App (AppEnv (..), StorageBackend (..), appEffectHandlers)
import Plutus.PAB.Core hiding (runPAB)
import qualified Plutus.PAB.Core as Core (runPAB)
import Plutus.PAB.Effects.Contract.Builtin
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Plutus.PAB.Monitoring.Config (defaultConfig)
import qualified Plutus.PAB.Monitoring.Monitoring as LM
import Plutus.PAB.Types hiding (defaultConfig, endpointTimeout)
import Schema (FormSchema)
import Transient.Console
import Transient.Move.Internals
import Transient.Move.Utils
import Transient.Move.Web
import Wallet.Emulator.Types (Wallet (..), WalletId)

import Effects



type PAB a = PABC GuessGameContracts a

runPAB :: MonadIO m => PAB a -> m a
runPAB = liftIO . runPABC


instance ToRest WalletId


mainseq = do
  handlers <- testnetHandlers
  let walletid = fromJust $ decode $ "\"17abbab9d64a5e1f8b267da9eda630d5de9c969c\""
  Core.runPAB def handlers $ do
    liftIO getChar

    cid <- activateContract (Wallet walletid) Lock
    liftIO $ print ("ACTIVATED", cid)
    r <- callEndpointOnInstance' cid "lock" LockParams {secretWord = "42", amount = Ada.adaValueOf $ fromIntegral 10} -- ,lockIndex=0}
    liftIO $ print ("RRRR=", r)
    liftIO getChar
    cid <- activateContract (Wallet walletid) Guess
    callEndpointOnInstance cid "guess" GuessParams {guessWord = "42"}

  liftIO getChar

instance Default WalletId where
  def = fromJust $ decode "\"17abbab9d64a5e1f8b267da9eda630d5de9c969c\"" -- a testnet wallet

wallet2 = "0c2317b3ba9e5d7f351df5122d13a3c488672a82"

instance Default Wallet where
  def = Wallet def

main = keep $
  initNode $ do
    initHandlers
    getWallet
    gameSequence <|> published "guess" <|> balances <|> save
    return ()
    
  where
    getWallet = do
      POSTData (wallet :: WalletId) <- minput "wallet" ("enter your wallet number" :: String)
      localIO $ print ("WALLET", wallet)
      local $ setSessionState $ Wallet wallet

    gameSequence = do
      POSTData (amo :: Int, word :: String, hint :: String) <- minput "lock" ("enter lock amount, the key and a hint. Example: 100 myKey \"word of 5 letters\"" :: String)
      cid <- pabLock amo word

      i <- local genPersistId
      guessw :: String <- public "guess" $ minput ("guess" <> show i) ("guess " <> hint)
      localIO $ print guessw
      pabGuess word guessw

    pabLock amo word= local $ do
      wallet <- getSessionState
      runPAB $ do
        cid <- activateContract wallet Lock
        callEndpointOnInstance cid "lock" LockParams {secretWord = word, amount = Ada.adaValueOf $ fromIntegral amo} -- ,lockIndex=0}
        return cid

    pabGuess word guessw =  do
      local $ do
        wallet <- getSessionState
        liftIO $ print ("wallet", wallet)
        runPAB $ do
          cid <- activateContract wallet Guess
          callEndpointOnInstance cid "guess" GuessParams {guessWord = guessw} --,guessIndex=0}
      minput "" (if word == guessw then "YES" else "NO" :: String)

    balances = do
      minput "bal" ("wallet data" :: String) :: Cloud ()
      Wallet walletid <- local $ getSessionState <|> error "no wallet?"
      snap <- getWalletUtxoSnapshot walletid
      minput "" snap :: Cloud ()
      empty

    initHandlers= local $ do
          handlers <- testnetHandlers
          initPAB handlers

    save = local $ do
        option ("save" :: String) "save execution state"
        liftIO syncCache
        empty

testnetHandlers :: MonadIO m => m (EffectHandlers (Builtin GuessGameContracts) (AppEnv GuessGameContracts))
testnetHandlers = liftIO $ do
  config' <- decodeFileThrow "pab-config.yml"
  let config = config' {nodeServerConfig = (nodeServerConfig config') {mscPassphrase = Just "pab123456789"}}
  def <- defaultConfig
  (trace, switchboard) <- setupTrace_ def "pab"

  let ccaTrace = convertLog PrettyObject trace
  -- (trace :: Trace IO (PrettyObject (AppMsg (Builtin a))), switchboard)
  -- print config
  return $ appEffectHandlers BeamSqliteBackend config (toPABMsg ccaTrace) handleBuiltin
  where
    -- toPABMsg :: Trace m (LM.AppMsg (Builtin a)) -> Trace m (LM.PABLogMsg (Builtin a))
    toPABMsg = LM.convertLog LM.PABMsg

data GuessGameContracts
  = Lock
  | Guess
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance Pretty GuessGameContracts where
  pretty = viaShow

instance HasDefinitions GuessGameContracts where
  getDefinitions = [Lock, Guess]
  getContract = getGuessGameContracts
  getSchema = getGuessGameSchema

getGuessGameSchema :: GuessGameContracts -> [FunctionSchema FormSchema]
getGuessGameSchema = \case
  Lock -> Builtin.endpointsToSchemas @GameSchema
  Guess -> Builtin.endpointsToSchemas @GameSchema

getGuessGameContracts :: GuessGameContracts -> SomeBuiltin
getGuessGameContracts = \case
  Lock -> SomeBuiltin $ (lock :: Promise () GameSchema T.Text ())
  Guess -> SomeBuiltin $ (guess :: Promise () GameSchema T.Text ())

