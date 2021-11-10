{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

import           Game hiding(game)
import           Control.Monad.Freer              -- (Eff, LastMember, Member, interpret, run, runM, send, sendM,
                                                  --  type (~>))
import qualified Control.Monad.Freer.Error    as Error
import qualified Control.Monad.Freer.Extras.Modify as Modify
import qualified Control.Monad.Freer.Extras.Log as Log
import           Control.Monad.Freer.Writer 
import           Control.Monad.Freer.Coroutine     (Yield,yield,runC,Status(..))
import           Control.Monad.Freer.NonDet     (NonDet,makeChoiceA)
import           Control.Monad.Freer.State 

import           Control.Monad.IO.Class            (liftIO)
import           Data.Aeson                        (FromJSON, ToJSON (toJSON))
import qualified Data.Aeson                        as JSON
import qualified Data.Aeson.Encode.Pretty          as JSON
import           Data.Bifunctor                    (bimap)
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Char8             as BS8
import qualified Data.ByteString.Lazy              as BSL
import           Data.Foldable                     (traverse_)
import           Data.Proxy                        (Proxy (..))
import           Data.Row.Extras                   (type (.\\))
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import           Options.Applicative               (CommandFields, Mod, Parser, ParserResult, command, disambiguate,
                                                    execParserPure, fullDesc, helper, idm, info, prefs, progDesc,
                                                    showHelpOnEmpty, showHelpOnError, subparser)
import qualified Options.Applicative
import           Playground.Schema                 (EndpointToSchema, endpointsToSchemas)
import           Plutus.Contract                   (Contract(..), EmptySchema,AsContractError(..))
import           Plutus.Contract.Effects
import           Plutus.Contract.Types             (handleContractEffs,ContractEffs)
import qualified Plutus.Contract.State    as ContractState         

import qualified Ledger.Ada                         as Ada


import           Plutus.Contract.Checkpoint
import           Plutus.Contract.Resumable         (Requests,ReqMap,RequestID,IterationID,Request,ResumableEffs,Resumable(..),handleResumable,suspendNonDet,MultiRequestContStatus)
import           Prelude                           hiding (getContents)
import           System.Environment                (getArgs)
import           System.Exit                       (ExitCode (ExitFailure), exitSuccess, exitWith)
import qualified System.IO

import Data.Map  as M
-- import Control.Monad.Trans.Accum (runAccumT)

import Plutus.Trace.Effects.RunContract       (callEndpoint)
import Unsafe.Coerce
--Create a command line application from a @Contract schema Text ()@. For example:

-- game :: AsContractError e => Contract GameSchema e ()
-- game = lock <|> guess

instance AsContractError () where
    _ContractError = error "error"


runContractEffs1 ::
 forall w  effs a.
 Monoid w
 => Eff (Error.Error ()
    ':  Log.LogMsg JSON.Value
    ':  Writer w
    ':  Checkpoint
    ':  '[Resumable PABResp PABReq])
    -- ': Yield PABReq PABResp
    -- ': ResumableEffs PABResp PABReq '[] a) 
     a ->    --Control.Monad.Freer.Coroutine.Status
                            -- '[] PABReq PABResp (Either () a, w)
        -- (Either () a, w)
        (Maybe(Plutus.Contract.Resumable.MultiRequestContStatus
                                PABResp PABReq '[] a)) 

runContractEffs1= undefined

-- runContractEffs :: forall w  effs a.
--  Monoid w
--  => Eff
--     (Error.Error ()
--         : Log.LogMsg JSON.Value : Writer w : Checkpoint
--         -- : Resumable PABResp PABReq  -- : Yield PABReq PABResp
--     --   : ResumableEffs PABResp PABReq '[] a)
--         : NonDet
--         : State IterationID
--         : State RequestID
--         : State (ReqMap PABResp PABReq '[] a)
--         : '[State (Requests PABReq)])
--     a
--     -> Eff '[] (Maybe (MultiRequestContStatus PABResp PABReq '[] a))
    -- -> Eff '[] (Status
    --         '[]
    --         PABResp
    --         PABReq
    --         (Maybe (MultiRequestContStatus PABResp PABReq '[Yield PABResp PABReq] a)))

runContractEffs :: 
   Eff
                        (Resumable PABResp PABReq
                           : Yield PABReq PABResp : ResumableEffs PABResp PABReq effs a)
                           a -> Eff effs (Maybe (MultiRequestContStatus PABResp PABReq effs a))
runContractEffs=
    -- runC 
    suspendNonDet  @PABResp @PABReq 
    . handleResumable @PABResp @PABReq


runContractb :: Eff
                        (Error.Error ()
                           : Log.LogMsg JSON.Value : Writer w : Checkpoint : effs)
                        x
                      -> Eff effs x
runContractb=
    handleCheckpointIgnore
    . runWriteIgnore 
    . Log.handleLogIgnore  @JSON.Value
    . handleError @()
    -- . runError @()

runWriteIgnore= interpret $ \case
   Tell _ -> error "Tell"

handleError:: forall e effs a.(Show e) => Eff (Error.Error e ': effs) a -> Eff effs a
handleError = interpret $ \case 
   Error.Error e  -> error $ show e

handleResumable1 ::
    forall i o effs.
    ( Member (Yield o i) effs
    -- , Member NonDet effs
    )
    => Eff (Resumable i o ': effs)
    ~> Eff effs
handleResumable1 = interpret $ \case
    RRequest o -> yield o id
    RSelect    -> error "RSelect"

-- | Ignore all log messages.
handleCheckpointIgnore :: Eff (Checkpoint  ': effs) ~> Eff effs
handleCheckpointIgnore = interpret $ \case
    DoCheckpoint -> pure()
    AllocateKey -> error "AllocatKey"
    Store _ _ _ -> error "Store"
    Retrieve _ -> error "Retrieve"

handleResumableIgnore :: Eff (Resumable i o ': effs) ~> Eff effs
handleResumableIgnore= interpret $ \case
    RRequest o -> error "RRequest"
    RSelect    -> error "RSelect"

{-
runc x= 
    let r = 
         run . 
         runState (CheckpointStore M.empty) . 
         runState (CheckpointKey 0) .
         runState (AccumState ())
         Log.handleLogIgnore @CheckpointLogMsg
         Log.handleLogIgnore @JSON.Value
         
         handleContractEffs $ Error.runError $ unContract x 
    in r `seq` return r
-}
-- main= do
--     let r = run . runState (1 :: Int)  $  (get :: Eff '[State Int] Int)
--     print r

main3=do
    r <- runc $ lock "pepe" $ Ada.lovelaceValueOf 100 
    print r
    -- r' <- runc $ guess "pepe"
    -- print r'
    
