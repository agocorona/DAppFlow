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

--Indexed

-- (FromJSON, ToJSON)

-- (Builtin, BuiltinHandler, contractHandler, handleBuiltin)

-- import Ledger.Constraints (mustPayToPubKey)

-- import Plutus.Contract.Request (Endpoint, awaitTxConfirmed, endpoint, submitTx)

-- (EffectHandlers (..), PABAction, PABEffects, PABEnvironment (..), handleContractDefinitionEffect, handleContractEffect, handleContractStoreEffect, handleLogMessages)

-- import Plutus.PAB.Simulator as Simulator
import Cardano.BM.Setup (setupTrace_)
import Cardano.Node.Types (MockServerConfig (..))
--   ( SimulatorContractHandler,
--   logBalances,
--   currentBalances,
--   logString,
--   SimulatorEffectHandlers,
--     runSimulationWith,
--     SimulatorState,
--     activateContract,
--     callEndpointOnInstance,
--     waitForEndpoint,
--     mkSimulatorHandlers,
--     waitNSlots
--   )
-- import Wallet.Emulator.Types (knownWallet)

-- (PABError)

-- , loadConfig)

-- Safe (SomeException(SomeException))

import Cardano.Wallet.Api.Types (ApiWalletUtxoSnapshot (..))
import ContractExample.GuessGame
import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent.STM as STM
import Control.Exception (SomeException)
import Control.Monad
import Control.Monad.Freer
import qualified Control.Monad.Freer.Error as Error
import Control.Monad.Freer.Extras.Log
import Control.Monad.Freer.Reader hiding (local)
import Control.Monad.IO.Class
import Control.Monad.State hiding (state)
import DAppFlow.Wallet
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Default (Default (def))
import Data.IORef
import qualified Data.List as Ada
import qualified Data.Map as M
import Data.Maybe
import Data.Maybe (fromJust)
import Data.TCache as TC hiding (onNothing)
import Data.TCache.DefaultPersistence as TC
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Typeable
import Data.Yaml (decodeFileThrow)
import GHC.Generics (Generic)
import Ledger (Value, pubKeyHash, txId)
import qualified Ledger.Ada as Ada
import Playground.Types (FunctionSchema)
import Plutus.Contract.Types (Contract, Promise (..))
import Plutus.Monitoring.Util (PrettyObject (..), convertLog)
import Plutus.PAB.App (AppEnv (..), StorageBackend (..), appEffectHandlers)
import Plutus.PAB.Core hiding (runPAB)
import qualified Plutus.PAB.Core as Core (runPAB)
import qualified Plutus.PAB.Core.ContractInstance.STM as Instances
import Plutus.PAB.Effects.Contract.Builtin
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Plutus.PAB.Effects.TimeEffect (TimeEffect (..))
import Plutus.PAB.Monitoring.Config (defaultConfig)
import qualified Plutus.PAB.Monitoring.Monitoring as LM
import Plutus.PAB.Timeout (Timeout)
import Plutus.PAB.Types hiding (defaultConfig, endpointTimeout)
import Schema (FormSchema)
import System.IO.Unsafe
import Transient.Console
import Transient.Internals
import Transient.Logged
import Transient.Move.Internals
import Transient.Move.Utils
import Transient.Move.Web
import Transient.Parse
import Wallet.Emulator.Types (Wallet (..), WalletId)

-- type PABC contract a = Eff (PABEffects (Builtin contract) (Simulator.SimulatorState (Builtin contract))) a
type PABC contract a = PABAction (Builtin contract) (AppEnv contract) a

type PAB a = PABC GuessGameContracts a

runPAB :: MonadIO m => PAB a -> m a
runPAB = liftIO . runPABC

-- instance {-# Overlappable #-} ToJSON a=> Show a where
--   show = BS.unpack . toLazyByteString . serializetoJSON . toJSON

-- instance FromJSON a => Read a where
--   readsPrec _ _= error "not implemented read for FromJSON class"

-- instance {-# Overlappable #-} (Typeable a,ToJSON a, FromJSON a) => Loggable a where
--    serialize= serializetoJSON
--    deserialize =deserializeJSON

instance ToRest WalletId

-- runPABCore :: MonadIO m => Timeout -> EffectHandlers (Builtin GuessGameContracts) (AppEnv GuessGameContracts) -> PAB a  -> IO (Either PABError a)
-- runPABCore= Core.runPAB

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
-- simulatorHandlers :: EffectHandlers (Builtin GuessGameContracts) (SimulatorState (Builtin GuessGameContracts))
-- simulatorHandlers = mkSimulatorHandlers def def handler
--   where
--     handler :: SimulatorContractHandler (Builtin GuessGameContracts)
--     handler = interpret (contractHandler handleBuiltin)

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

{-
data TestContracts
  = GuessGame
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Pretty TestContracts where
  pretty = viaShow

instance HasDefinitions TestContracts where
  getDefinitions = [GuessGame]

  getSchema = \case
        GuessGame  ->  Builtin.endpointsToSchemas @GameSchema

  getContract _ = SomeBuiltin endpoints'
-}

-- endpoints' :: Contract () GameSchema T.Text ()
-- endpoints'= endpoints >> endpoints'

{-# NOINLINE state #-}
state = unsafePerformIO $ newIORef $ error "no PAB state. please run initPAB first"

initPAB :: MonadIO m => EffectHandlers t env -> m ()
initPAB effectHandlers = liftIO $ do
  let EffectHandlers
        { initialiseEnvironment,
          onStartup
        } = effectHandlers
  let endpointTimeout = def
  (instancesState, blockchainEnv, appEnv) <- runM . Main.handleError @PABError $ initialiseEnvironment
  writeIORef state $ PABEnvironment {instancesState, blockchainEnv, appEnv, effectHandlers, endpointTimeout}
  runPABC onStartup

handleError :: forall e effs a. (Show e) => Eff (Error.Error e ': effs) a -> Eff effs a
handleError = interpret $ \case
  Error.Error e -> error $ show e

-- | Get a 'PABRunner' that uses the current environment.
runPABC :: forall t env a. PABAction t env a -> IO a -- (Either PABError a)
runPABC action = do
  h@PABEnvironment {effectHandlers = EffectHandlers {handleLogMessages, handleContractStoreEffect, handleContractEffect, handleContractDefinitionEffect}} <- readIORef state -- ask @(PABEnvironment t env)
  r <-
    runM $
      Error.runError $
        runReader h $
          interpret (handleTimeEffect @t @env) $
            handleLogMessages $
              handleContractDefinitionEffect $
                handleContractEffect $
                  handleContractStoreEffect action
  case r of
    Left x -> error $ show x
    Right x -> return x

-- From Core

-- | Handle the 'TimeEffect' by reading the current slot number from
--   the blockchain env.
handleTimeEffect ::
  forall t env m effs.
  ( Member (Reader (PABEnvironment t env)) effs,
    LastMember m effs,
    MonadIO m
  ) =>
  TimeEffect
    ~> Eff effs
handleTimeEffect = \case
  SystemTime -> do
    Instances.BlockchainEnv {Instances.beCurrentSlot} <- liftIO $ readIORef state >>= return . blockchainEnv -- asks @(PABEnvironment t env) blockchainEnv
    liftIO $ STM.readTVarIO beCurrentSlot
