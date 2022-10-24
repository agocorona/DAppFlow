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
import Cardano.Node.Types (PABServerConfig (..))
import Auction.EnglishAuction
import Control.Applicative

import Control.Monad.IO.Class
import DAppFlow.Wallet
import Data.Aeson
import Data.Default (Default (def))
import Data.Maybe

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
import Transient.Internals
import Transient.Logged(Raw(..))
import Transient.Move.Internals
import Transient.Move.Utils
import qualified Transient.Move.Services
import Transient.Move.Web
import Transient.Parse
import Wallet.Emulator.Types (Wallet (..), WalletId)

import Effects

import qualified Data.Map as M
import Data.IORef
import Data.ByteString.Lazy.Char8 hiding (head,empty)


type PAB a = PABC AuctionContracts a

runPAB :: MonadIO m => PAB a -> m a
runPAB = liftIO . runPABC


instance ToRest WalletId

instance Default WalletId where
  def = fromJust $ decode "\"17abbab9d64a5e1f8b267da9eda630d5de9c969c\"" -- a testnet wallet

wallet2 = "0c2317b3ba9e5d7f351df5122d13a3c488672a82"

instance Default Wallet where
  def = Wallet Nothing def






main = keep $ initNode $ do
    initHandlers
    getWallet
    auctionSequence <|> published "bids"  <|> balances <|> save
    return ()
    
  where
    getWallet = do
      POSTData (alias :: String, wallet :: WalletId) <- minput "wallet" ("enter your alias and wallet number" :: String)
      localIO $ print ("WALLET", wallet)
      local $ setSessionState $ Wallet Nothing wallet

    auctionSequence = do
      auction <- startIt
      waitClose auction <|> public "bids" (bidIt auction) 
      return()
      where
      startIt= do
        POSTData(deadline,minbid,currency,token) <- minput "start" ("Enter 4 start parameters" :: String)
        local $ do
          let startparams= StartParams deadline minbid currency token
          wallet <- getSessionState
          runPAB $ do 
            cid <- activateContract wallet Start
            callEndpointOnInstance cid "start"  startparams
            return startparams

      bidIt auction= do
        bid <- minput "start" $ ("Enter bid  for: currency: " <> (show $ spCurrency auction) <> "   token: " <> (show $ spToken auction)  :: String)
        local $ do
          wallet <- getSessionState
          runPAB $ do 
            cid <- activateContract wallet Bid
            callEndpointOnInstance cid "bid" $ BidParams (spCurrency auction) (spToken auction) bid
    
      waitClose auction= local $ do
        waait $ deadLine auction
        runPAB close $ CloseParams (spCurrency auction) (spToken auction)
     

    balances = do
      minput "bal" ("wallet data" :: String) :: Cloud ()
      Wallet _ walletid <- local $ getSessionState <|> error "no wallet?"
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

testnetHandlers :: MonadIO m => m (EffectHandlers (Builtin AuctionContracts) (AppEnv AuctionContracts))
testnetHandlers = liftIO $ do
  config' <- decodeFileThrow "pab-config.yml"
  let config = config' {nodeServerConfig = (nodeServerConfig config') {pscPassphrase = Just "pab123456789"}}
  defConf <- defaultConfig
  (trace, _) <- setupTrace_ defConf "pab"

  let ccaTrace = convertLog PrettyObject trace
  -- (trace :: Trace IO (PrettyObject (AppMsg (Builtin a))), switchboard)
  -- print config
  return $ appEffectHandlers BeamSqliteBackend config (toPABMsg ccaTrace) handleBuiltin
  where
    -- toPABMsg :: Trace m (LM.AppMsg (Builtin a)) -> Trace m (LM.PABLogMsg (Builtin a))
    toPABMsg = LM.convertLog LM.PABMsg

data AuctionContracts
  = Start
  | Bid
  | Close
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance Pretty AuctionContracts where
  pretty = viaShow

-- instance HasDefinitions AuctionContracts where
--   getDefinitions = [Start, Bid, Close]
--   getContract = auctionContracts
--   getSchema = getAuctionSchema

-- getAuctionSchema :: AuctionContracts -> [FunctionSchema FormSchema]
-- getAuctionSchema = \case
--   Start -> Builtin.endpointsToSchemas @GameSchema
--   Bid -> Builtin.endpointsToSchemas @GameSchema
--   Close -> Builtin.endpointsToSchemas @GameSchema

-- auctionContracts :: AuctionContracts -> SomeBuiltin
-- auctionContracts = \case
--   Start -> SomeBuiltin $ (start :: Promise () GameSchema T.Text ())
--   Bid -> SomeBuiltin $ (bid :: Promise () GameSchema T.Text ())
--   Close -> SomeBuiltin $ (close :: Promise () GameSchema T.Text ())
