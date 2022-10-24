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

module Effects where


import Control.Concurrent.STM as STM
import Control.Monad.Freer
import qualified Control.Monad.Freer.Error as Error
import Control.Monad.Freer.Reader hiding (local)
import Control.Monad.IO.Class
import Data.Default (Default (def))
import Data.IORef
import Plutus.PAB.Core hiding (runPAB)
import qualified Plutus.PAB.Core.ContractInstance.STM as Instances
import Plutus.PAB.Effects.TimeEffect (TimeEffect (..))
import Plutus.PAB.Types hiding (defaultConfig, endpointTimeout)
import System.IO.Unsafe
import Plutus.PAB.Effects.Contract.Builtin
import Plutus.PAB.App (AppEnv (..), StorageBackend (..), appEffectHandlers)


{-# NOINLINE state #-}

-- type PABC contract a = Eff (PABEffects (Builtin contract) (Simulator.SimulatorState (Builtin contract))) a
type PABC contract a = PABAction (Builtin contract) (AppEnv contract) a

state = unsafePerformIO $ newIORef $ error "no PAB state. please run initPAB first"

initPAB :: MonadIO m => EffectHandlers t env -> m ()
initPAB effectHandlers = liftIO $ do
  let EffectHandlers
        { initialiseEnvironment,
          onStartup
        } = effectHandlers
  let endpointTimeout = def
  (instancesState, blockchainEnv, appEnv) <- runM . Effects.handleError @PABError $ initialiseEnvironment
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


