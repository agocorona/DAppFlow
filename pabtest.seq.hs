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


import ContractExample.GuessGameIndexed
import Control.Concurrent.STM as STM
import Control.Monad.Freer
import qualified Control.Monad.Freer.Error as Error
import Control.Monad.Freer.Reader hiding (local)
import Control.Monad.IO.Class
import Control.Monad
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

import Transient.Internals
import Transient.Move.Internals
import Transient.Move.Utils
import Control.Applicative
import Data.ByteString.Lazy.Char8 as BS (unpack)

type PABC contract a = Eff (PABEffects (Builtin contract) (Simulator.SimulatorState (Builtin contract))) a

type PAB a = PABC GuessGameContracts a


runPAB :: MonadIO m => PAB a -> m a
runPAB =  liftIO . runPABC




main3 = Simulator.runSimulationWith handlers $ do

     cid  <- Simulator.activateContract (Wallet 1) Lock
     cid2 <- Simulator.activateContract (Wallet 2) Lock
     cid3 <- Simulator.activateContract (Wallet 3) Guess
     cid4 <- Simulator.activateContract (Wallet 4) Guess


     waitNSlots 3
     callEndpointOnInstance  cid "lock" LockParams{secretWord="world", amount= Ada.adaValueOf 200,lockIndex=1}
     waitNSlots 3
     callEndpointOnInstance  cid2 "lock" LockParams{secretWord="world2", amount= Ada.adaValueOf 200,lockIndex=2}
     waitNSlots 3
     callEndpointOnInstance  cid3 "guess" GuessParams{guessWord="world",guessIndex=1}
     waitNSlots 3
     Simulator.logString @(Builtin GuessGameContracts) "SENDING SECOND GUESS"

     callEndpointOnInstance  cid3 "guess" GuessParams{guessWord="world2",guessIndex=1}
     Simulator.logString @(Builtin GuessGameContracts) "END SENDING SECOND GUESS"

     waitNSlots 10
     Simulator.logString @(Builtin GuessGameContracts) "Balances at the end of the simulation"
     b <- Simulator.currentBalances
     Simulator.logBalances @(Builtin GuessGameContracts) b


  where
  handlers :: SimulatorEffectHandlers (Builtin GuessGameContracts)
  handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler (Builtin.handleBuiltin @GuessGameContracts))


locks = unsafePerformIO $ newIORef ([] :: [InputData])

main= keep $ initNode $  do
    local $ initPAB simulatorHandlers

    firstCont
    
    wallet <- minput "wallet" enter "your wallet id" 
    local $ setState wallet
    
    gameSequence   <|> availableOptions <|>  balances

  where

  gameSequence = do
    (amo ::Int,word, hint) <-  minput "lock" "enter lock amount, the key and a hint. Example: 100 myKey \"number between 0..1000\""
    local $ do
      wallet <- getState
      runPAB $ do
         cid  <- runPAB $ Simulator.activateContract (Wallet wallet) Lock
         callEndpointOnInstance  cid "lock" LockParams{secretWord=BS.unpack word, amount= Ada.adaValueOf $ fromIntegral amo,lockIndex=ind}
         return()

    minput "guess" ("guess a lock for " ++ hint) <|> addToOptions :: Cloud ()
    
    local $ do
        wallet <- getState
        runPAB $ do
            cid3 <-  Simulator.activateContract (Wallet wallet) Guess
            callEndpointOnInstance  cid3 "guess" GuessParams{guessWord=guessw,guessIndex=index}
            waitNSlots 3

  availableOptions = local $ do
      inputdatas <- liftIO $ readIORef locks -- `onNothing`  return [] -- do minput "" "no lock has been done yet. Do it yourself!"  ; empty
      foldr (<|>) empty $ map (\(InputData msg url) -> sendURL msg url) inputdatas

  balances= do
    minput "bal" "display account balances" :: Cloud ()
    local $ runPAB $ do
      Simulator.logString @(Builtin  GuessGameContracts) "Balances at the end of the simulation"
      b <-  Simulator.currentBalances
      Simulator.logBalances @(Builtin GuessGameContracts) b
      return()

  addToOptions :: Loggable a => Cloud a
  addToOptions=  local $ do
        idata <- getState
        liftIO $ atomicModifyIORef' locks (\idatas -> (idata:idatas,())) 
        empty

simulatorHandlers :: EffectHandlers (Builtin GuessGameContracts) (SimulatorState (Builtin GuessGameContracts))
simulatorHandlers = mkSimulatorHandlers def def handler
  where
    handler :: SimulatorContractHandler (Builtin GuessGameContracts)
    handler = interpret (contractHandler handleBuiltin)

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

endpoints' :: Contract () GameSchema T.Text ()
endpoints'= endpoints >> endpoints'



{-# NOINLINE state #-}
state = unsafePerformIO $ newIORef undefined

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
