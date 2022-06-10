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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}



import ContractExample.GuessGame --Indexed
import DAppFlow.Wallet
import Control.Concurrent.STM as STM
import Control.Monad.Freer
import qualified Control.Monad.Freer.Error as Error
import Control.Monad.Freer.Reader hiding (local)
import Control.Monad.IO.Class
import Control.Monad
import Data.Aeson -- (FromJSON, ToJSON)
import Data.Default (Default (def))
import Data.IORef
-- (Builtin, BuiltinHandler, contractHandler, handleBuiltin)

import Data.Text.Prettyprint.Doc
import GHC.Generics (Generic)
import Ledger (Value, pubKeyHash, txId)
import qualified Ledger.Ada                         as Ada

-- import Ledger.Constraints (mustPayToPubKey)
import Playground.Types (FunctionSchema)
-- import Plutus.Contract.Request (Endpoint, awaitTxConfirmed, endpoint, submitTx)
import Plutus.Contract.Types (Promise (..),Contract)
import Cardano.Node.Types(MockServerConfig(..))
import Plutus.PAB.Core hiding (runPAB)  -- (EffectHandlers (..), PABAction, PABEffects, PABEnvironment (..), handleContractDefinitionEffect, handleContractEffect, handleContractStoreEffect, handleLogMessages)
import qualified Plutus.PAB.Core as Core (runPAB) 
import qualified Plutus.PAB.Core.ContractInstance.STM as Instances
import Plutus.PAB.Effects.Contract.Builtin
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Plutus.PAB.Effects.TimeEffect (TimeEffect (..))
import Plutus.PAB.App(AppEnv(..),StorageBackend(..),appEffectHandlers)
-- import Plutus.PAB.Simulator as Simulator
import Cardano.BM.Setup (setupTrace_)
import  Control.Monad.Freer.Extras.Log

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
import Wallet.Emulator.Types (Wallet (..),WalletId)

import Plutus.PAB.Types  hiding (endpointTimeout,defaultConfig) -- (PABError)
import Plutus.Monitoring.Util(convertLog,PrettyObject(..))
import Plutus.PAB.Monitoring.Config (defaultConfig) -- , loadConfig)
import Plutus.PAB.Timeout (Timeout)

import qualified Plutus.PAB.Monitoring.Monitoring  as LM


import Schema (FormSchema)
import System.IO.Unsafe
import qualified Data.Text                        as T
import qualified Data.List as Ada

import Transient.Internals
import Transient.Move.Internals
import Transient.Move.Utils
import Transient.Logged
import Transient.Parse
import Transient.Console
import Transient.Move.Web
import Control.Applicative
import Data.ByteString.Lazy.Char8 as BS (unpack)
import Data.Typeable
import Data.Yaml(decodeFileThrow)
import Control.Exception(SomeException) -- Safe (SomeException(SomeException))
import Data.Maybe(fromJust)
import Control.Concurrent.MVar
import Control.Monad.State hiding(state)
import Cardano.Wallet.Api.Types(ApiWalletUtxoSnapshot(..))

-- type PABC contract a = Eff (PABEffects (Builtin contract) (Simulator.SimulatorState (Builtin contract))) a
type PABC contract a = PABAction (Builtin contract) (AppEnv contract) a

type PAB a = PABC GuessGameContracts a


runPAB :: MonadIO m => PAB a -> m a
runPAB =  liftIO . runPABC



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

main=  do
  handlers <- testnetHandlers
  let walletid= fromJust $ decode $    "\"17abbab9d64a5e1f8b267da9eda630d5de9c969c\""
  Core.runPAB def handlers $ do
       liftIO  getChar

       cid  <- activateContract (Wallet walletid) Lock
       liftIO $ print ("ACTIVATED",cid)
       r <- callEndpointOnInstance'  cid "lock" LockParams{secretWord="42", amount= Ada.adaValueOf $ fromIntegral 1000} -- ,lockIndex=0}
       liftIO $ print ("RRRR=",r) 

  tempty <-newEmptyMVar
  takeMVar tempty

  {-
  handlers <- testnetHandlers
  initPAB handlers
  let wallet= fromJust $ decode $    "\"0c2317b3ba9e5d7f351df5122d13a3c488672a82\""
  liftIO $ print wallet

  runPAB $ do
    cid  <- activateContract (Wallet wallet) Lock
    callEndpointOnInstance  cid "lock" LockParams{secretWord="42", amount= Ada.adaValueOf $ fromIntegral 1000} -- ,lockIndex=0}
  
  tempty <-newEmptyMVar
  takeMVar tempty
  -}

   {-
   
  let EffectHandlers
          { initialiseEnvironment,
            onStartup
          } = handlers
    let endpointTimeout = def
  (instancesState, blockchainEnv, appEnv) <- runM . Main.handleError @PABError $ initialiseEnvironment

  h@PABEnvironment {effectHandlers = EffectHandlers {handleLogMessages, handleContractStoreEffect, handleContractEffect, handleContractDefinitionEffect}} <- readIORef state -- ask @(PABEnvironment t env)
    r <-
      runM $
        Error.runError $
          runReader h $
            interpret (handleTimeEffect @t @env) $
              handleLogMessages $
                handleContractDefinitionEffect $
                  handleContractEffect $
                    handleContractStoreEffect $ do
            let wallet= fromJust $ decode $    "\"17abbab9d64a5e1f8b267da9eda630d5de9c969c\""
            liftIO $ print wallet
            cid  <- activateContract (Wallet wallet) Lock
            callEndpointOnInstance  cid "lock" LockParams{secretWord="42", amount= Ada.adaValueOf $ fromIntegral 100} -- ,lockIndex=0}

    print x


    
    tempty <-newEmptyMVar
    takeMVar tempty
   -}
  
instance Default WalletId where
  def =  fromJust $ decode "\"17abbab9d64a5e1f8b267da9eda630d5de9c969c\""  -- a testnet wallet

instance Default Wallet where
  def = Wallet def

str= "00F0\r\n{\"entries\":[{\"ada\":{\"unit\":\"lovelace\",\"quantity\":996931858},\"ada_minimum\":{\"unit\":\"lovelace\",\"quantity\":999978},\"assets\":[]},{\"ada\":{\"unit\":\"lovelace\",\"quantity\":1000000000},\"ada_minimum\":{\"unit\":\"lovelace\",\"quantity\":999978},\"assets\":[]}]}\r\n0\r\n\r\n"

str2= "0001\r\n2\r\n0\r\n\r\n"

main1= keep $  do

  withParseString "333" $ int
  -- snap <- withParseString str $ dechunk |- deserialize :: TransIO ApiWalletUtxoSnapshot

  snap <- runCloud $ getWalletUtxoSnapshot def

  liftIO $ print snap
  
main2= keep $ initNode $ do
  local $ setSessionState  (def :: Wallet)
  balances

mainweb = keep $ initNode $  do

    local $  do
      handlers <- testnetHandlers
      initPAB handlers

    POSTData (wallet :: WalletId) <- minput "wallet" ("enter your wallet number" :: String)

    local $ setSessionState $ Wallet wallet

    gameSequence <|> availableOptions <|>  balances

  where
  -- rind= unsafePerformIO $ newIORef 0


  gameSequence =  do
    POSTData(amo ::Int,word :: String, hint :: String) <-  minput "lock" ("enter lock amount, the key and a hint. Example: 100 myKey \"word of 5 letters\"" :: String)
    tr "after lock"
    (ind,cid) <- local $ do
      wallet <- getSessionState
      -- ind <- liftIO $ atomicModifyIORef rind $ \i -> (i+1,i+1)
      -- onException $ \(e :: SomeException) -> do liftIO $ print  ("ERRORRRRRRRRRRRR",e); empty
      cid <- runPAB $ do
              cid  <- activateContract  wallet Lock
              callEndpointOnInstance  cid "lock" LockParams{secretWord=word, amount= Ada.adaValueOf $ fromIntegral amo} -- ,lockIndex=0}
              return cid
      tr "CIR RETURNEDDDDDDDDDDDDDDDDDDDDDDDDDDDD"
      return (0 :: Int,cid)

    guessw :: String <- minput "guess" ("guess " <> hint) <|> addToOptions
    localIO $ print guessw
    local $ do

      wallet <- getSessionState
      liftIO $ print ("wallet", wallet)
      runPAB $ do
          cid  <-  activateContract (Wallet wallet)  Guess
          callEndpointOnInstance  cid "guess" GuessParams{guessWord=guessw} --,guessIndex=0}
          -- waitNSlots 3

    minput ""  (if word== guessw then "YES" else "NO" :: String)

  availableOptions =  do
      inputdatas <-  local getRState
      local $ foldr (<|>) empty $ map (\(InputData id msg url) -> sendOption msg url) inputdatas

  addToOptions :: Loggable a => Cloud a
  addToOptions=  do
        idata :: InputData  <- local getState
        lcks :: [InputData] <- local getRState
        local $ setRState $ idata:lcks
        empty


balances= do
    minput "bal" ("wallet data" :: String) :: Cloud ()
    Wallet walletid <- local $ getSessionState <|> error "no wallet?"
    snap <- getWalletUtxoSnapshot walletid
    -- snap <- callGoogle "Transient.Base"
    localIO $ print snap
    -- local $ runPAB $ do
    --   Simulator.logString @(Builtin  GuessGameContracts) "Balances at the end of the simulation"
    --   b <-  Simulator.currentBalances
    --   Simulator.logBalances @(Builtin GuessGameContracts) b
      -- return()
    


-- simulatorHandlers :: EffectHandlers (Builtin GuessGameContracts) (SimulatorState (Builtin GuessGameContracts))
-- simulatorHandlers = mkSimulatorHandlers def def handler
--   where
--     handler :: SimulatorContractHandler (Builtin GuessGameContracts)
--     handler = interpret (contractHandler handleBuiltin)


testnetHandlers :: MonadIO m => m (EffectHandlers (Builtin GuessGameContracts) (AppEnv GuessGameContracts))
testnetHandlers= liftIO $ do
    config' <- decodeFileThrow "pab-config.yml"
    let config= config' { nodeServerConfig   = (nodeServerConfig  config') { mscPassphrase = Just "pab123456789"}}
    def <-  defaultConfig
    (trace , switchboard) <- setupTrace_ def "pab"

    let ccaTrace = convertLog PrettyObject trace
    -- (trace :: Trace IO (PrettyObject (AppMsg (Builtin a))), switchboard)
    -- print config
    return $ appEffectHandlers BeamSqliteBackend config (toPABMsg ccaTrace) handleBuiltin
    where
    -- toPABMsg :: Trace m (LM.AppMsg (Builtin a)) -> Trace m (LM.PABLogMsg (Builtin a))
    toPABMsg = LM.convertLog LM.PABMsg

data GuessGameContracts = Lock
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
    Lock   -> Builtin.endpointsToSchemas @GameSchema
    Guess  -> Builtin.endpointsToSchemas @GameSchema

getGuessGameContracts :: GuessGameContracts -> SomeBuiltin
getGuessGameContracts = \case
    Lock   -> SomeBuiltin $ (lock :: Promise () GameSchema T.Text ())
    Guess  -> SomeBuiltin $ (guess :: Promise () GameSchema T.Text ())
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
    Instances.BlockchainEnv {Instances.beCurrentSlot} <- liftIO $ readIORef state -- asks @(PABEnvironment t env) blockchainEnv
    liftIO $ STM.readTVarIO beCurrentSlot
