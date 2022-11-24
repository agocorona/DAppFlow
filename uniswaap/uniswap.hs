
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
{-# LANGUAGE OverloadedStrings,ImportQualifiedPost,StandaloneDeriving #-}

import  Plutus.Contracts.Uniswap as Uniswap
import Data.Map as Map hiding (empty)
import Plutus.PAB.Simulator qualified as Simulator
import Data.Aeson -- (FromJSON, Result (..), ToJSON, encode, fromJSON)
import Data.Semigroup qualified as Semigroup
import Plutus.Contracts.Currency qualified as Currency
import Plutus.Contracts.Uniswap qualified as Uniswap
import Plutus.Contracts.Uniswap.Trace as US
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), HasDefinitions (..), SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Simulator (SimulatorEffectHandlers, logString)
import Plutus.Contract.Types(awaitPromise)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.OpenApi.Schema qualified as OpenApi
import Prettyprinter (Pretty (..), viaShow)
import Data.Monoid qualified as Monoid
import Ledger.Ada (adaSymbol, adaToken)
import qualified Plutus.PAB.Core.ContractInstance.STM as Instances

import Wallet.Emulator.Types (Wallet (..))
import Data.IORef

import Transient.Base
import Transient.Move.Utils
import Transient.Move
import Transient.Move.Web
-- import Transient.Move.IPFS
import Transient.Console
import Control.Applicative
-- import Effects
import Plutus.PAB.Core hiding (runPAB)
import Control.Monad.IO.Class
import Control.Monad
import Wallet.Emulator.Types (knownWallet)
import Control.Concurrent.STM as STM
import Data.Default
import System.IO.Unsafe
import Plutus.V1.Ledger.Value(TokenName)
-- import Cardano.Wallet.Primitive.Types(WalletId)
import Wallet.Types (ContractInstanceId)
import Data.Typeable


--for SimulatorEffects
import Control.Monad.Freer
import qualified Control.Monad.Freer.Error as Error
import Control.Monad.Freer.Reader hiding (local)
import Plutus.PAB.Effects.TimeEffect (TimeEffect (..))
import Plutus.PAB.Types (PABError)




type PAB a = PABC UniswapContracts a

type PABC contract a = Eff (PABEffects (Builtin contract) (Simulator.SimulatorState (Builtin contract))) a


runPAB :: MonadIO m => PAB a -> m a
runPAB = liftIO . runPABC

data UniswapContracts =
      Init
    | UniswapStart
    | UniswapUser Uniswap.Uniswap
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty UniswapContracts where
    pretty = viaShow

instance HasDefinitions UniswapContracts where
    getDefinitions = [Init, UniswapStart]
    getSchema = \case
        UniswapUser _ -> Builtin.endpointsToSchemas @Uniswap.UniswapUserSchema
        UniswapStart  -> Builtin.endpointsToSchemas @Uniswap.UniswapOwnerSchema
        Init          -> Builtin.endpointsToSchemas @Builtin.Empty
    getContract = \case
        UniswapUser us -> SomeBuiltin . awaitPromise $ Uniswap.userEndpoints us
        UniswapStart   -> SomeBuiltin Uniswap.ownerEndpoint
        Init           -> SomeBuiltin US.setupTokens

simulatorHandlers :: EffectHandlers (Builtin UniswapContracts) (Simulator.SimulatorState (Builtin UniswapContracts))
simulatorHandlers = Simulator.mkSimulatorHandlers  def handler
  where
    handler :: Simulator.SimulatorContractHandler (Builtin.Builtin UniswapContracts)
    handler = interpret (contractHandler Builtin.handleBuiltin)


deriving instance ToJSONKey TokenName
deriving instance FromJSONKey TokenName

data AllCoins=AllCoins (Map TokenName (Coin B)) (Coin A) deriving (Typeable, Generic, FromJSON, ToJSON)
data WalletContract= WalletContract  Integer ContractInstanceId deriving (Typeable, Generic, FromJSON, ToJSON)


-- main= keep $ initNode $ do
--     local $ do
--         setIPFS
--         initPAB simulatorHandlers
--         runPAB $ do




main= keep $ initNode $ do
    local $ do
        -- setIPFS
        initPAB simulatorHandlers
    (us,cs) <- factoryCreate <|> balances


    let coins = Map.fromList [(tn, Uniswap.mkCoin cs tn) | tn <- tokenNames]
        ada   = Uniswap.mkCoin adaSymbol adaToken

    local $ setState (AllCoins coins ada)
    enterWallet us
    uniswapFlow  us  <|> published "swap" <|> published ("addPool") 

balances= local $ do
    option ("bal" :: String) "display account balances" 
    runPAB $ do
      Simulator.logString @(Builtin  UniswapContracts) "Balances at the end of the simulation"
      b <-  Simulator.currentBalances
      Simulator.logBalances @(Builtin UniswapContracts) b
    empty

uniswapFlow  us =do
  WalletContract w cid <- local  getSessionState
  pool <- createPool w cid 
  public "addPool"  (addPool pool) <|> closeFactory us
  removePool pool <|> public "swap" swapAtoB  <|> public "swap" swapBtoA  
  return()

factoryCreate=  local $ do
    -- option ("create" ::String) "create a factory"
    abduce
    runPAB $do
        cidInit  <- Simulator.activateContract (knownWallet 1) Init
        cs       <- flip Simulator.waitForState cidInit $ \json -> case fromJSON json of
                    Success (Just (Semigroup.Last cur)) -> Just $ Currency.currencySymbol cur
                    _                                   -> Nothing
        void $ Simulator.waitUntilFinished cidInit
        logString @(Builtin UniswapContracts) $ "Initialization finished. Minted: " ++ show cs



        cidStart <- Simulator.activateContract (knownWallet 1) UniswapStart
        us       <- flip Simulator.waitForState cidStart $ \json -> case (fromJSON json :: Result (Monoid.Last (Either Text Uniswap.Uniswap))) of
                        Success (Monoid.Last (Just (Right us))) -> Just us
                        _                                       -> Nothing
        logString @(Builtin UniswapContracts) $ "Uniswap instance created: " ++ show us
        

        

        -- setState us
        return (us,cs)


enterWallet us= do
    walletid <- minput "wallet" ("enter wallet number 1-10" :: String)
    let wallet= knownWallet walletid
    cid <- local $ runPAB $ do
        cid <- Simulator.activateContract wallet $ UniswapUser us
        logString @(Builtin UniswapContracts) $ "Uniswap user contract started for " ++ show walletid
        Simulator.waitForEndpoint cid "funds"
        void $ Simulator.callEndpointOnInstance cid "funds" ()
        v <- flip Simulator.waitForState cid $ \json -> 
               case (fromJSON json :: Result (Monoid.Last (Either Text Uniswap.UserContractState))) of
                Success (Monoid.Last (Just (Right (Uniswap.Funds v)))) -> Just v
                _                                                      -> Nothing
        logString @(Builtin UniswapContracts) $ "initial funds in wallet " ++ show walletid ++ ": " ++ show v
        return cid
    setSessionState $ WalletContract walletid cid


createPool w cid2 = do
    AllCoins coins ada <- local  getState
    (adaamo :: Integer,coinAmo :: Integer) <- minput "create" ("create Pool: give me your combination of Ada/CoinA, for example 100000/500000 " :: String) 
    local $ do
        -- Wallet w <- getSessionState
        -- let ada   = Uniswap.mkCoin adaSymbol adaToken
        let cp = Uniswap.CreateParams ada (coins Map.! "A") (fromIntegral adaamo) (fromIntegral coinAmo)

        runPAB $ do
            logString @(Builtin UniswapContracts) $ "creating liquidity pool: " ++ show (encode cp)
            Simulator.waitForEndpoint cid2 "create"
            void $ Simulator.callEndpointOnInstance cid2 "create" cp
            flip Simulator.waitForState cid2 $ \json -> case (fromJSON json :: Result (Monoid.Last (Either Text Uniswap.UserContractState))) of
                Success (Monoid.Last (Just (Right Uniswap.Created))) -> Just ()
                _                                                    -> Nothing
            logString @(Builtin UniswapContracts) "liquidity pool created"

addPool= undefined
closeFactory _= undefined :: Cloud ()
removePool= undefined

swapBtoA= error "swapBtoA not implemented"
swapAtoB   = do
    AllCoins coins ada <- local getState <|> (error "getState swapAtoB")
    ammo <- minput "swapAB" ("amount of ADA to exchange" :: String) 
    swapCoins ada (coins Map.! "A")  (Amount ammo) 0

swapCoins coinA coinB amountA amountB= local $ do
    WalletContract walletid cid <- getRState
    runPAB $ callEndpointOnInstance cid "swap" (SwapParams coinA coinB amountA amountB)


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