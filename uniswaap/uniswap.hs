
import  Plutus.Contracts.Uniswap as Uniswap


main= keep $ initNode $ do
    local $ do
        setIPFS
        let coins = Map.fromList [(tn, Uniswap.mkCoin cs tn) | tn <- tokenNames]
        ada   = Uniswap.mkCoin adaSymbol adaToken
        setState (coins,ada)
    factoryCreate 
    enterWallet
    uniswapFlow <|> published "swap" <|> published ("addPool")

uniswapFlow=do
  pool <- createPool 
  public "addPool"  (addPool pool) <|> closeFactory uniswap
  removePool pool <|> public "swap" (swapAtoB pool amountA) <|> public "swap" (swapBtoA pool amountB) 

factoryCreate= do
    option "create" "create a factory"
    uniswap <- runPAB start
    setState uniswap

enterWallet= do
    wallet <- minput "w" "enter wallet"
    setSessionState $ Wallet wallet

createPool= do
    minput "create" "create Pool, give "
    Wallet w <- getSessionState
    us <- activateContract (knownWallet 1) UniswapStart
    runPAB $ do
        cid <- activateContract w $ UniswapUser us
        v <- callEndpointOnInstance cid "funds" ()
        out $ "initial funds in wallet " ++ show w ++ ": " ++ show v

        let cp = Uniswap.CreateParams ada (coins Map.! "A") 100000 500000
        logString @(Builtin UniswapContracts) $ "creating liquidity pool: " ++ show (encode cp)
        -- let cid = cids Map.! knownWallet 2
        Simulator.waitForEndpoint cid2 "create"
        _  <- Simulator.callEndpointOnInstance cid2 "create" cp
        flip Simulator.waitForState (cids Map.! knownWallet 2) $ \json -> case (fromJSON json :: Result (Monoid.Last (Either Text Uniswap.UserContractState))) of
            Success (Monoid.Last (Just (Right Uniswap.Created))) -> Just ()
        _                                                    -> Nothing
    logString @(Builtin UniswapContracts) "liquidity pool created"

swapAtoB pool amountA= swap coins Map.! "A" ada

swapCoins= coinA coinB amountA amountB= runPAB $ do
    callEndpontInInstance swap us (SwapParams coinA coinB amountA amounB)


tokenNames :: [TokenName]
tokenNames = ["A", "B", "C", "D"]