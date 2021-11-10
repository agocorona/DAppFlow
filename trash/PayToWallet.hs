{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module ContractExample.PayToWallet
  ( payToWallet,
    funds,
    PayToWalletParams (..),
    PayToWalletSchema,
    FundsSchema
    -- payToWalletEndpoints,
  )
where

import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import qualified Data.Semigroup as Semigroup
import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import GHC.Generics (Generic)
import Ledger (Value, pubKeyHashAddress, txId, txOutTxOut, txOutValue)
import Ledger.Constraints
import Ledger.Crypto (pubKeyHash)
import Ledger.Value as Value
import Plutus.Contract
import qualified Plutus.Contracts.Currency as Currency
import Plutus.PAB.Effects.Contract.Builtin as Builtin
import qualified Plutus.V1.Ledger.Api as P
import Schema (ToSchema)
import Wallet.Emulator.Types (Wallet (..), walletPubKey)
import Debug.Trace

-- import Plutus.V1.Ledger.Value

data PayToWalletParams = PayToWalletParams
  { amount :: Value,
    wallet :: Wallet
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type FundsSchema = Endpoint "funds" ()

type PayToWalletSchema = Endpoint "Pay to wallet" PayToWalletParams 


-- payToWalletEndpoints = selectList [funds,payToWallet] >> payToWalletEndpoints

-- | Gets the caller's funds.
funds :: Promise () PayToWalletSchema ContractError  Int
funds = endpoint @"funds" $ \_ -> do
  return 1
  -- pkh <- pubKeyHash <$> ownPubKey
  -- os <- map snd . Map.toList <$> utxoAt (pubKeyHashAddress pkh)
  -- trace "FUNDS" $ return()
  -- return $ mconcat [txOutValue $ txOutTxOut o | o <- os]

payToWallet :: Promise () PayToWalletSchema ContractError ()
payToWallet = endpoint @"Pay to wallet" $ \PayToWalletParams {amount, wallet} -> do
  let pkh = pubKeyHash $ walletPubKey wallet
  txid <- submitTx (mustPayToPubKey pkh amount)
  awaitTxConfirmed (txId txid)
  trace "PAYTOWALLET" $ return()
