module DAppFlow.Wallet where

import Cardano.Wallet.Api.Types
import Wallet.Emulator.Types (WalletId)

import Transient.Move
import Transient.Logged (Raw(..))
import Transient.Move.Services
import Data.Aeson(decode)

import Data.ByteString.Lazy.Char8 as BS
import Control.Monad.IO.Class

getWallet :: WalletId -> Cloud ApiWallet
getWallet wid= do
        Raw mr <- callService (walletHeader "GET /v2/wallets/$1") wid
        case decode mr of
                Just r -> return r
                Nothing -> error $ "getWallet error for: " <> show wid


getWalletUtxoSnapshot :: WalletId -> Cloud ApiWalletUtxoSnapshot
getWalletUtxoSnapshot wid=  callService (walletHeader "GET /v2/wallets/$1/utxo") wid
        -- Raw mr <- callService (walletHeader "GET /v2/wallets/$1/utxo") wid
        -- let r = decode mr
        -- case r of
        --         Just r -> return r
        --         Nothing -> error $ "getWallet error for: " <> show wid

walletHeader req= [("service","Cardano wallet"),("type","HTTP")
        ,("nodehost","localhost")
        ,("nodeport","8090")
        ,("HTTPstr",req ++ " HTTP/1.1\r\nHost: $hostnode\r\n\r\n")]

callGoogle :: BS.ByteString -> Cloud BS.ByteString
callGoogle mod=  callService google mod
google= [("service","google"),("type","HTTP")
        ,("nodehost","www.google.com")
        ,("nodeport","80")
        ,("HTTPstr","GET /search?q=+$1+site:hackage.haskell.org HTTP/1.1\r\nHost: $hostnode\r\n\r\n" )]