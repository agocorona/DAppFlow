Write Plutus smart contracts and DApps as continuous workflows.
===============================================================

Problem statement
-----------------

DApp developpers maintainers and verifiers face the "endpoint hell" in Web, in chain and in out-chain code


Summary of the solution to the problem
---------------------------------------

- Express the entire program flow within a Haskell monad which set HTTP endpoints for microservices and uses Plutus endpoints.
- DApps involving contracts take a long time to complete. some intended or unintended shutdowns of the application may happen. The program support stop and recovery of the execution state at the step where it was when it was stopped. 


The monad will execute as off-chain Haskell code. It will be a continuous `do` expression that will define HTTP endpoints and will use Cardano endpoints. The result is a clear specification of the entire process as a "workflow" which is clearly readable, maintainable and verifiable.

Example
-------

At this moment, the file [pabtest.hs](https://github.com/agocorona/DAppFlow/blob/main/pabtest.hs) contains a very simple example of how a continuous workflow using the verbs `lock` and `guess` taken from the basic example of contract [guessgame](https://github.com/input-output-hk/plutus-starter/blob/main/examples/src/Plutus/Contracts/Game.hs) slightly modified for allowing multiple locks could be integrated in a continuous workflow, using the plutus simulator:

*NOTE*  A more sequential version of this same game is in the second snippet that shows better the advantages of a multiuser workflow expressed in a single monadic (imperative-like) expression.

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
    local $ do
      wallet <- getState
      runPAB $ do
         cid  <- runPAB $ Simulator.activateContract (Wallet wallet) Lock
         callEndpointOnInstance  cid "lock" LockParams{secretWord=BS.unpack word, amount= Ada.adaValueOf $ fromIntegral amo,lockIndex=ind}
         return()

  guesses=do
    minput "guess" "guess a lock"  :: Cloud ()
    ind <- localIO $ readIORef rind

    if ind==0 then minput "" "no lock has been done yet. Do it yourself!" else do
      (index,guessw)  <- foldr (<|>) empty $ map (\i -> (,) <$> return i <*> minput ("g" <> show i) ("guess "++ show i)) [1..ind]
      local $ do
        wallet <- getState
        runPAB $ do
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

A better way
------------

Now, lets write the game in a more sequential way. A person would describe the game as "first someone lock some amount and a key, then  any other could guess that key and receive the money. That sequentiality is expressend in  `gameSequence` below where `lock` and `guess` are.. humm.., in sequence:

```haskell
main = keep $
  initNode $ do
    initHandlers

    getWallet

    gameSequence <|> published "guess" <|> balances 
    return ()

  where
    getWallet = do
      POSTData (wallet :: WalletId) <- minput "wallet" ("enter your wallet number" :: String)
      localIO $ print ("WALLET", wallet)
      local $ setSessionState $ Wallet wallet

    gameSequence = do
      POSTData (amo :: Int, word :: String, hint :: String) <- minput "lock" ("enter lock amount, the key and a hint. Example: 100 myKey \"word of 5 letters\"" :: String)
      cid <- pabLock amo word

      i <- local genPersistId
      guessw :: String <- public "guess" $ minput ("guess" <> show i) ("guess " <> hint)
      localIO $ print guessw
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
  Wallet walletid <- local $ getSessionState <|> error "no wallet?"
  snap <- getWalletUtxoSnapshot walletid
  minput "" snap :: Cloud ()
  empty

initHandlers= local $ do
      handlers <- testnetHandlers
      initPAB handlers
```

After locking a number, the same user can guess his own lock. That is nice but it is not a multiuser workflow where any other user can guees the other's lock. That is the role of `public`  and `published` which add the guess link to a list and exposes all the guess links to other users who entered in the application and created locks.  

this line...

```haskell
 guessw <- public "guess" $ minput "guess" ("guess a lock for " ++ hint) 
```

..means: show a guess link to the user that sent the lock, but at the same time before that user respond, add this guess link to the options that every other user get once he enter his wallet.

So we have a multinuser workflow in which one user locks and every other can guess. 

Naturally,  by changing having different identifiers for `public` and `published` you can program more personalized options.

Note that we don´t have to store the variables like the word to gues etc in a off-chain session state, since they are local variables in the monad. In general, there is no need to manage database objects. It is stored with the state of the program, which can be stored in IPFS registers.This simplifies the development. 

As a detail, in guess, now there are two wallets involved in the lock-guess game: the one of the locker and the one of the guesser. the first can be retrived with `getState`; the second with `getSessionState`. The second gives the data of the user who executed a te previous `minput`.


An example interaction of this second snippet is:

```
option: start
hostname of this node. (Must be reachable, default:localhost)? "localhost"
if you want to retry with port+1 when the port is busy, write 'retry': 
port to listen? 8000
Connected to port: "8000"
Enter  id               to: enter your wallet number       url:    http://localhost:8000/2/20002000/0/0/1/$int

```
In another console:

```


curl 'http://localhost:8000/2/20002000/0/0/1'
[{ "msg"=enter lock amount, the key and a hint. Example: 100/myKey/\"number between 0..1000\""
, "url"="http://localhost:8000/23/40002000/0/0/$int/$int/String"}

# curl 'http://localhost:8000/7/40002000/0/0/100/42/"number between 0..1000"'
[{ "msg"="guess number between 0..1000", "url"="http://localhost:8000/17/70002000/0/0/$int"}]
```
Enter another wallet number

```
curl 'http://localhost:8000/2/20002000/0/0/1/2/'
[{ "msg"="enter a lock number", "url"="http://localhost:8000/19/40002000/0/0/$int"}
,{ "msg"=""guess number between 0..1000", "cont"=http://localhost:8000/17/70002000/0/0/$int}]
```

Now "juan", besided starting a new game, has received one more option above: to guess the number entered by the first wallet. $int means a number which is the key entered previously (42).

```
# curl 'http://localhost:8000/9/70002000/0/0/42/'
[{ "msg"="YES", "url"="http://localhost:8000/21/110002000/0/0/"}]

```
He entered 42 and yes that was the number. The on-chain code will reflect it in the balances.

The on-chain code is not shown here, but it is identical of the one of the  plutus canonical game example with the addition 
of a index to allow differnt games at the same time. See [guessGameIndexed.hs](https://github.com/agocorona/DAppFlow/blob/main/ContractExample/GuessGameIndexed.hs) and compare it with [guessGame.hs](https://github.com/agocorona/DAppFlow/blob/main/ContractExample/GuessGame.hs)

Status
======

Currently this is under development ad I can not write a recipe for easy compilationand excecution of the examples



