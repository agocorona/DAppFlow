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


import ContractExample.GuessGame
import Control.Concurrent.STM as STM
import Control.Monad.Freer
import qualified Control.Monad.Freer.Error as Error
import Control.Monad.Freer.Reader
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON)
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
import Plutus.PAB.Core (EffectHandlers (..), PABAction, PABEffects, PABEnvironment (..), handleContractDefinitionEffect, handleContractEffect, handleContractStoreEffect, handleLogMessages)
import qualified Plutus.PAB.Core.ContractInstance.STM as Instances
import Plutus.PAB.Effects.Contract.Builtin
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Plutus.PAB.Effects.TimeEffect (TimeEffect (..))
import Plutus.PAB.Simulator as Simulator
  ( SimulatorContractHandler,
  logBalances,
  currentBalances,
  logString,
  SimulatorEffectHandlers,
    runSimulationWith,
    SimulatorState,
    activateContract,
    callEndpointOnInstance,
    waitForEndpoint,
    mkSimulatorHandlers,
    waitNSlots
  )
import Plutus.PAB.Types (PABError)
import Schema (FormSchema)
import System.IO.Unsafe
import Wallet.Emulator.Types (Wallet (..))
import qualified Data.Text                        as T
import qualified Data.List as Ada



type PABC contract a = Eff (PABEffects (Builtin contract) (Simulator.SimulatorState (Builtin contract))) a

type PAB a = PABC TestContracts a


runPAB :: PAB a -> IO a
runPAB = runPABC




main1 = Simulator.runSimulationWith handlers $ do

     cid <- Simulator.activateContract (Wallet 1) GuessGame

     waitNSlots 3
     callEndpointOnInstance cid "lock" LockParams{secretWord="world", amount= Ada.adaValueOf 200}

     waitNSlots 3
     callEndpointOnInstance cid "guess" GuessParams{guessWord="world"}
     waitNSlots 3
     Simulator.logString @(Builtin TestContracts) "Balances at the end of the simulation"
     b <- Simulator.currentBalances
     Simulator.logBalances @(Builtin TestContracts) b  

  
  where
  handlers :: SimulatorEffectHandlers (Builtin TestContracts)
  handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler (Builtin.handleBuiltin @TestContracts))


main = do
  initPAB simulatorHandlers
  cid  <- runPAB $ Simulator.activateContract (Wallet 1) GuessGame
  cid2 <- runPAB $ Simulator.activateContract (Wallet 2) GuessGame 

  runPAB $  waitNSlots 3

  runPAB $ callEndpointOnInstance cid "lock" LockParams{secretWord="world", amount= Ada.adaValueOf 200}
  runPAB $ waitNSlots 3

  runPAB $ callEndpointOnInstance cid2 "guess" GuessParams{guessWord="world"}
  runPAB $ waitNSlots 3
  runPAB $ do
     Simulator.logString @(Builtin TestContracts) "Balances at the end of the simulation"
     b <- Simulator.currentBalances
     Simulator.logBalances @(Builtin TestContracts) b  

simulatorHandlers :: EffectHandlers (Builtin TestContracts) (SimulatorState (Builtin TestContracts))
simulatorHandlers = mkSimulatorHandlers def def handler
  where
    handler :: SimulatorContractHandler (Builtin TestContracts)
    handler = interpret (contractHandler handleBuiltin)

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

endpoints' :: Contract () GameSchema T.Text ()
endpoints'= endpoints >> endpoints'
  










{-# NOINLINE state #-}
state = unsafePerformIO $ newIORef undefined

initPAB :: EffectHandlers t env -> IO ()
initPAB effectHandlers = do
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
