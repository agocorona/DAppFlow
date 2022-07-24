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
import ContractExample.GuessGame
import Control.Applicative

import Control.Monad.IO.Class
import DAppFlow.Wallet
import Data.Aeson
import Data.Default (Default (def))
import Data.Maybe
import Data.TCache as TC hiding (onNothing)
import Data.TCache.DefaultPersistence as TC
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
import qualified Transient.Move.Services (callService)
import Transient.Move.Web
import Transient.Parse
import Wallet.Emulator.Types (Wallet (..), WalletId)

import Effects

import qualified Data.Map as M
import Data.IORef
import Data.ByteString.Lazy.Char8 hiding (head,empty)


type PAB a = PABC GuessGameContracts a

runPAB :: MonadIO m => PAB a -> m a
runPAB = liftIO . runPABC


instance ToRest WalletId


mainseq = do
  handlers <- testnetHandlers
  let walletid = fromJust $ decode $ "\"17abbab9d64a5e1f8b267da9eda630d5de9c969c\""
  Core.runPAB def handlers $ do
    liftIO getChar

    cid <- activateContract (Wallet Nothing walletid) Lock
    liftIO $ print ("ACTIVATED", cid)
    r <- callEndpointOnInstance' cid "lock" LockParams {secretWord = "42", amount = Ada.adaValueOf $ fromIntegral 10} -- ,lockIndex=0}
    liftIO $ print ("RRRR=", r)
    liftIO getChar
    cid <- activateContract (Wallet Nothing walletid) Guess
    callEndpointOnInstance cid "guess" GuessParams {guessWord = "42"}

  liftIO getChar

instance Default WalletId where
  def = fromJust $ decode "\"17abbab9d64a5e1f8b267da9eda630d5de9c969c\"" -- a testnet wallet

wallet2 = "0c2317b3ba9e5d7f351df5122d13a3c488672a82"

instance Default Wallet where
  def = Wallet Nothing def




{-
curl -X POST "http://127.0.0.1:5001/api/v0/key/gen?arg=testname"
curl -X POST "http://127.0.0.1:5001/api/v0/cat?arg=QmcP6mZWbgQSegoJZW7m65jKxCZAZRThqP1doCWVDHxHpi"


-}

ipfsHeader req body= [("service","IPFS"),("type","HTTP")
        ,("nodehost","localhost")
        ,("nodeport","5001")
        ,("HTTPstr",req <> " HTTP/1.1\r\nHost: $hostnode\r\nContent-Length: "<> show (Prelude.length body)<>"\r\n\r\n" <> body)]

ipfsCat = ipfsHeader "GET /api/v0/cat?arg=$1" ""

ipfsAdd= undefined

ipfsAddmUpload body = [("service","IPFS"),("type","HTTP")
        ,("nodehost","localhost")
        ,("nodeport","5001")
        ,("HTTPstr","POST /api/v0/add HTTP/1.1\r\n" <>
              "Host: localhost\r\n" <>
              "Content-Type: multipart/form-data; boundary=---------------------------735323031399963166993862150\r\n"<>
              "Content-Length: "<> show (Prelude.length body1)<>"\r\n\r\n" <> body1)]
   where
   body1= tcachedir <> body <> end
   end=  "-----------------------------735323031399963166993862150--"

   tcachedir=  "-----------------------------735323031399963166993862150\r\n"<>

      "Content-Disposition: form-data; name=\"file\"; filename=\"tcachedata\"\r\n" <>
      "Content-Type: application/x-directory\r\n\r\n" 

{-
salvar al final.
solo almacenar el hash de tcachedata

-}

addFile name content=
  "-----------------------------735323031399963166993862150\r\n"<>
  "Content-Disposition: form-data; name=\""<>name<>"\"; filename=\"tcachedata%2F"<>name<>"\"\r\n"<>
  "Content-Type: text/plain\r\n\r\n"<>content

main= keep' $ do

    Raw r <- callService (ipfsAddmUpload $ addFile "hello" "hello content\r\n" <> addFile "word" "word content\r\n" ) ()
    liftIO $ print ("RESULLLLLT",r)

rest=

  -- "-----------------------------735323031399963166993862150\r\n"<>

  -- "Content-Disposition: form-data; name=\"file\"; filename=\"tcachedata\"\r\n" <>
  -- "Content-Type: application/x-directory\r\n\r\n" <>

  "-----------------------------735323031399963166993862150\r\n"<>
  "Content-Disposition: form-data; name=\"file1\"; filename=\"tcachedata%2Fa.txt\"\r\n"<>
  "Content-Type: text/plain\r\n\r\n"<>

  "Content of a.txt modified.\r\n\r\n"<>

  "-----------------------------735323031399963166993862150\r\n" <>
  "Content-Disposition: form-data; name=\"file2\"; filename=\"tcachedata%2Fa.html\"\r\n"<>
  "Content-Type: text/html\r\n\r\n" <>

  "<!DOCTYPE html><title>Content of a.html.</title>\r\n\r\n"<>

  "-----------------------------735323031399963166993862150\r\n"<>
  "Content-Disposition: form-data; name=\"file3\"; filename=\"tcachedata%2Fbinary\"\r\n"<>
  "Content-Type: application/octet-stream\r\n\r\n"<>

  "aωb\r\n"   -- <>
  -- "-----------------------------735323031399963166993862150--"
      

ipnsKeyGen = ipfsHeader "GET /api/v0/key/gen?arg=$1" ""

ipnsResolve = ipfsHeader "GET /api/v0/name/resolve?arg=$1" ""

ipnsPublish = ipfsHeader "GET /api/v0/name/publish?arg=$1&key=$2" ""

callService s p= runCloud $ Transient.Move.Services.callService s p

jsonFilter field reg= withParseString reg $ do
   tDropUntilToken $ "\""<> field <> "\":\""
   tTakeWhile (/='\"')

setIPFS mid= local $ do
      (ipnsid, lockupt) <- if isJust mid 
        then  do
         return (fromJust mid,M.empty) 
         ipfs  <- callService ipnsResolve (fromJust mid) >>=  jsonFilter "Path"
         table <- callService ipfsCat ipfs >>= return . deserialize
         return (fromJust mid,table)
        else do
         ipnsid <- callService ipnsKeyGen ("lockuptable" :: String) >>=  jsonFilter "Id"
         return (ipnsid,M.empty)

     
      
      rindex <- liftIO $ newIORef  lockupt
      let iPFSPersist= TC.Persist {
          readByKey = \k -> do
            r <- keep' $ do
              let mipns = M.lookup k lockupt
              if isNothing mipns then return Nothing
              else callService ipfsCat (fromJust mipns) 
            return $ head r,
          write = \k content -> do
                    keep' $ do  -- atomicModifyIORef $ rindex $ \rfs -> ((k,content):rfs,()),
                             ipfsid <- callService ipfsAdd(k,content) >>= jsonFilter "Hash"
                             liftIO $ atomicModifyIORef rindex $ \lock -> (M.insert k ipfsid lock,())
                    return(),

          delete = \k -> print ("deleting",k)
          }
      liftIO $ TC.setDefaultPersist iPFSPersist
      fork $ saveIndex ipnsid rindex
  where
  saveIndex ipnsid rindex= do
    react'  setCond  -- activated after each cycle of saving modified registers of the cache
    liftIO $ print "SAVE IPFS INDEX"
    index <- liftIO $ readIORef rindex
    ipfsid <- callService ipfsAdd ("lockuptable" :: String,index) >>=  jsonFilter "Hash" 
    callService ipnsPublish (ipnsid, ipfsid) :: TransIO ()
    return()
    where
    -- callback to set up an action when the writing of registers finish
    setCond :: (() -> IO ()) -> IO ()
    setCond fx= TC.setConditions (return ())  (fx ())
    
    react' mx= react mx (return())




main2 = keep $ initNode $ do
    setIPFS Nothing
    initHandlers
    getWallet
    gameSequence <|> published "guess" <|> balances <|> save
    return ()
    
  where
    getWallet = do
      POSTData (wallet :: WalletId) <- minput "wallet" ("enter your wallet number" :: String)
      localIO $ print ("WALLET", wallet)
      local $ setSessionState $ Wallet Nothing wallet

    gameSequence = do
      POSTData (amo :: Int, word :: String, hint :: String) <- minput "lock" ("enter lock amount, the key and a hint. Example: 100 myKey \"word of 5 letters\"" :: String)
      cid <- pabLock amo word

      i <- local genPersistId
      guessw :: String <- public "guess" $ minput ("guess" <> show i) ("guess " <> hint)
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

testnetHandlers :: MonadIO m => m (EffectHandlers (Builtin GuessGameContracts) (AppEnv GuessGameContracts))
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

