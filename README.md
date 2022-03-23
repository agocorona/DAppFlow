Write Plutus smart contracts and DApps as continuous workflows.
===============================================================

Problem statement
-----------------

DApp developpers maintainers and verifiers face the "endpoint hell" in Web, in chain and in out-chain code


Summary of the solution to the problem
---------------------------------------

Express the entire program flow within a Haskell monad which set HTTP endpoints for microservices and uses Plutus endpoints
DApps involving contracts take a long time to complete. some intended or unintended shutdowns of the application may happen. The program support stop and recovery of the execution state at the step where it was when it was stopped. Also, individual Workflowa that are waiting can be freed from memory.


The monad will execute as off-chain Haskell code. It will be a continuous `do` expression that will define HTTP endpoints and will use Cardano endpoints. The result is a clear specification of the entire process as a "workflow" which is clearly readable, maintainable and verifiable.

Example
-------

At this moment, the file [pabtest.hs](https://github.com/agocorona/DAppFlow/blob/main/pabtest.hs) contains a very simple example of how a continuous workflow using the verbs `lock` and `guess` taken from the basic example of contract [guessgame](https://github.com/input-output-hk/plutus-starter/blob/main/examples/src/Plutus/Contracts/Game.hs) slightly modified for allowing multiple locks could be integrated in a continuous workflow, using the plutus simulator:

```haskell
main= keep $ initNode $  do
    local $ initPAB simulatorHandlers

    firstCont
    
    wallet <- minput "wallet" enter "your wallet id" 
    local $ setState wallet
    
    locks <|> guesses <|>  balances

  where

  rind= unsafePerformIO $ newIORef 0
  locks= do
    (amo ::Int,word) <-  minput "lock" "enter lock amount and the key. Example: 100 myKey"
    ind <- localIO $ atomicModifyIORef rind $ \i -> (i+1,i+1)
    local $ runPAB $ do
      wallet <- local getState
      cid  <- runPAB $ Simulator.activateContract (Wallet wallet) Lock
      callEndpointOnInstance  cid "lock" LockParams{secretWord=BS.unpack word, amount= Ada.adaValueOf $ fromIntegral amo,lockIndex=ind}
      return()

  guesses=do
    minput "guess" "guess a lock"  :: Cloud ()
    ind <- localIO $ readIORef rind

    if ind==0 then minput "" "no lock has been done yet. Do it yourself!" else do
      (index,guessw)  <- foldr (<|>) empty $ map (\i -> (,) <$> return i <*> minput ("g" <> show i) ("guess "++ show i)) [1..ind]
      local $ runPAB $ do
        wallet <- local getState
        cid3 <-  Simulator.activateContract (Wallet wallet) Guess
        callEndpointOnInstance  cid3 "guess" GuessParams{guessWord=guessw,guessIndex=index}

        waitNSlots 3
  
  balances= do
    minput "bal" "display account balances" :: Cloud ()
    local $ runPAB $ do
      Simulator.logString @(Builtin  GuessGameContracts) "Balances at the end of the simulation"
      b <-  Simulator.currentBalances
      Simulator.logBalances @(Builtin GuessGameContracts) b
      return()
```

In that example, some web/console endpoints (with `minput`) and in-chain plutus (with`callEndpointOnInstance`)  are invoqued. Since the "contract" in this case is trivial, with one step for each `lock` adn `guess`, it does not make evident the power of sequencing many interactions with the user and the on-chain code in the same monadic expression.

DApps involving contracts take a long time to complete. The program support stop and recovery of the execution state at the step where it was when it was stopped.

The program can be executed as a console application or as a HTTP server, using a HTTP client. The commands option and input mean that the endpoint at line 37 need two parameters to continue executing, The program can get them from the request URL, from the console if they are provided in the command line or interactively. This would facilitate testing.

Managed session state: Each endpoint has in scope all the variables computed in previous steps. The session state contains all these variables and will be stored in a file, in IPFS or in Cardano metadata. If the server for these particular endpoint is stopped, the execution state will be restored and the execution will continue.
