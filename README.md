Write Plutus smart contracts and DApps as continuous workflows.
===============================================================

Plutus is the smart contract part of the Cardano/Ada blockchain. Cardano is programmed larguely using the Haskell language. Plutus has a set of libraries and tools for the creation of smart contracts: Plutus Playground, the Plutus Application Backend (PAB) and Marlowe are the main pieces.

The PAB is the main target for programmers of Dapp applications with a server side. For light browser applications there are third-party general web services like blockfrost that hide the complexities of Plutus. DAppFlow is a library that facilitates the use case of the former, making the programmer's productivity and life as good or better, if possible, than the latter.

Problem statement
-----------------
There are important barriers for developers that face the development of Plutus Smart Contracts among them: 

 - The novelty and the rapid evolution of Plutus and Cardano, 
 - the complexity of the Haskell language, 
 - the inherent complexity of server applications with a complex state, 
 - the complexity of programs where different users interact,
 - the complexity of programs whose execution may last for a long time: months or years and
 - the added complexity of programs whose state evolves along different steps.
 - the need to interact with IPFS for a cloud-native stack all the way down, including the database 

The objective of DAppFlow is to mitigate them by means of a library which packs in a internal monad a lot of the features needed for the creation of DApps and  expose a reduced surface of primitives to the programmer.


Summary of the solution to the problem
---------------------------------------

- To allow the expression of the entire program flow within a Haskell monad. This monad will create transparently, the HTTP endpoints for Browser clients, respond to console commands, uses IPFS storage and controls state management. In the other side  it invokes the off-chain and on-chain Plutus code created as usual, with Plutus Playground or Marlowe.
- 
- DApps involving contracts take a long time to complete. some intended or unintended shutdowns of the application may happen. The program support transparent stop and recovery of the execution state at the step where it was when it was -intendedly or unintendedly- stopped. 

The monad will execute as off-chain Haskell code. It will be a continuous `do` expression that will define HTTP endpoints and will use Cardano endpoints. The result is a clear specification of the entire process as a "workflow" which is clearly readable, maintainable and verifiable.

Example
-------
I take the simplest example: the lock/guess game.

 A person would describe the game as "first someone lock some amount of money and a key, then any other could guess that key and receive the money. That sequentiality is expressed in  `gameSequence` below where `lock` and `guess` are.. humm.., in sequence. 

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
This program makes use of [GuessGame.hs](https://github.com/agocorona/DAppFlow/blob/main/ContractExample/GuessGame.hs) which defines the pab endpoints `guess` and `lock` It has been drawn from the public repository of Plutus and has been created using the plutus tools such is the Plutus Playground.

After locking a number, the same user can guess his own lock. That is nice but it is not a multiuser workflow where any other user can guees the other's lock. That is the role of `public`  and `published` which add the guess link to a list and exposes all the guess links to other users once they enter in the application.

this line...

```haskell
 guessw <- public "guess" $ minput "guess" ("guess a lock for " ++ hint) 
```

..means: show a guess link to the user and publish this link, so that every other user get it once he enter his wallet.

So we have a multiuser workflow in which one user locks and every other can guess. 

Naturally,  by having different identifiers for `public` and `published` you can program more personalized options.

Note that we don't need a database for state data; We don´t have to store the variables like the word to gues etc in a off-chain session state, since they are local variables in the monad. In general, there is no need to manage database objects. It is in the state of the whole thread, which can be stored in IPFS registers.This simplifies the development. 

As a detail, in guess, now there are two wallets involved in the lock-guess game: the one of the locker and the one of the guesser. the first can be retrived with `getState`; the second with `getSessionState`. The second gives the data of the user who executed a te previous `minput`.

There are many things to explain: `local` the code under `runPAB` etc. I can not explain it deeply here but let´s say that the first means that the code is executed in the local machine. It is necesary for logging and recovery of the thread state in this or other machine.  The code under runPAB invokes smart contract code that is- defined in "GuessGame.hs' and in the standard plutus PAB libraries.

Another good thing is that `minput` not only creates HTTP links, but also can work with console Input. 
So we have a console application for free, that can be useful for testing purposes.

An example interaction of this code is:

```
option: start
hostname of this node. (Must be reachable, default:localhost)? "localhost"
if you want to retry with port+1 when the port is busy, write 'retry': 
port to listen? 8000
Connected to port: "8000"
Enter  id               to: enter your wallet number       url:    http://localhost:8000/2/20002000/0/0/1/$int

```
$int means a number. In another console we enter the wallet number '1'.

```


curl 'http://localhost:8000/2/20002000/0/0/1'
[{ "msg"=enter lock amount, the key and a hint. Example: 100/myKey/\"number between 0..1000\""
, "url"="http://localhost:8000/23/40002000/0/0/$int/$int/String"}

entering 100 lovelaces as amount and 42 as the number:

# curl 'http://localhost:8000/7/40002000/0/0/100/42/"number between 0..1000"'
[{ "msg"="guess number between 0..1000", "url"="http://localhost:8000/17/70002000/0/0/$int"}]
```

We simulate another user by entering another wallet number '2'

```
curl 'http://localhost:8000/2/20002000/0/0/2/'
[{ "msg"="enter a lock number the key and a hint. Example: 100/myKey/\"number between 0..1000\""
 , "url"="http://localhost:8000/19/40002000/0/0/$int"}
,{ "msg"="guess number between 0..1000", "url"=http://localhost:8000/17/70002000/0/0/$int}]
```

Now the user of the wallet 2, besided starting a new game, has received one more option: to guess the number entered by the wallet 1. 
```

# curl 'http://localhost:8000/9/70002000/0/0/42/'
[{ "msg"="YES", "url"="http://localhost:8000/21/110002000/0/0/"}]

```
He entered 42 and yes that was the number. The on-chain code will reflect it in the balances.

# Advantages:

- Clear programming flow, following the specification
- Automatic state data management, no databases.
- Automatic restart of the program state on request
- Automatic HTTP interface generaton
- Automatic console program


Status
======

Currently this is under development ad I can not write a recipe for easy compilationand excecution of the examples.
IPFS serialization: first version working.



