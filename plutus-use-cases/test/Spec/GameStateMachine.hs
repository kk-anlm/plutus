{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Spec.GameStateMachine where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Log                                   (LogLevel (..))
import           Control.Monad.Freer.Writer
import           Data.List
import           Data.Map                                                  (Map)
import qualified Data.Map                                                  as Map
import           Data.Maybe
import           Data.Text.Prettyprint.Doc
import           Data.Void
import           Test.QuickCheck                                           as QC hiding ((.&&.))
import           Test.QuickCheck.Monadic
import           Test.Tasty                                                hiding (after)
import qualified Test.Tasty.HUnit                                          as HUnit

import qualified Spec.Lib                                                  as Lib

import qualified Language.PlutusTx                                         as PlutusTx

import           Language.Plutus.Contract.Test                             hiding (not)
import           Language.Plutus.Contract.Test.ContractModel
import           Language.Plutus.Contract.Test.StateModel                  (stateAfter)
import qualified Language.Plutus.Contract.Test.StateModel                  as StateModel
import           Language.PlutusTx.Coordination.Contracts.GameStateMachine as G
import           Language.PlutusTx.Lattice
import qualified Ledger.Ada                                                as Ada
import qualified Ledger.Typed.Scripts                                      as Scripts
import           Ledger.Value                                              (Value, isZero)
import           Plutus.Trace.Emulator                                     as Trace
import           Wallet.Emulator                                           (Wallet)
import           Wallet.Emulator.Folds                                     (postMapM)

-- * QuickCheck model

data GameModel = GameModel
    { _gameValue     :: Integer
    , _hasToken      :: Maybe Wallet
    , _currentSecret :: String }
    deriving (Show)

makeLenses 'GameModel

deriving instance Eq (HandleKey GameModel schema)
deriving instance Show (HandleKey GameModel schema)

instance ContractModel GameModel where

    type Err    GameModel = GameError

    data HandleKey GameModel schema where
        WalletKey :: Wallet -> HandleKey GameModel GameStateMachineSchema

    -- The commands available to a test case
    data Command GameModel = Lock      Wallet String Integer
                           | Guess     Wallet String String Integer
                           | GiveToken Wallet
        deriving (Eq, Show)

    initialState = GameModel
        { _gameValue     = 0
        , _hasToken      = Nothing
        , _currentSecret = ""
        }

    -- 'perform' gets a state, which includes the GameModel state, but also contract handles for the
    -- wallets and what the model thinks the current balances are.
    perform handle s cmd = case cmd of
        Lock w new val -> do
            callEndpoint @"lock" (handle $ WalletKey w)
                         LockArgs{lockArgsSecret = new, lockArgsValue = Ada.lovelaceValueOf val}
            delay 2
        Guess w old new val -> do
            callEndpoint @"guess" (handle $ WalletKey w)
                GuessArgs{ guessArgsOldSecret = old
                         , guessArgsNewSecret = new
                         , guessArgsValueTakenOut = Ada.lovelaceValueOf val}
            delay 1
        GiveToken w' -> do
            let w = fromJust (s ^. modelState . hasToken)
            payToWallet w w' gameTokenVal
            delay 1

    -- 'nextState' descibes how each command affects the state of the model

    nextState (Lock w secret val) = do
        hasToken      $= Just w
        currentSecret $= secret
        gameValue     $= val
        forge gameTokenVal
        deposit  w gameTokenVal
        withdraw w $ Ada.lovelaceValueOf val
        wait 2

    nextState (Guess w old new val) = do
        correct <- (old ==) <$> viewModelState currentSecret
        when correct $ do
            currentSecret $= new
            gameValue     $~ subtract val
            deposit w $ Ada.lovelaceValueOf val
        wait 1

    nextState (GiveToken w) = do
        w0 <- fromJust <$> viewModelState hasToken
        transfer w0 w gameTokenVal
        hasToken $= Just w
        wait 1

    -- To generate a random test case we need to know how to generate a random
    -- command given the current model state.
    arbitraryCommand s = oneof $
        [ Lock      <$> genWallet <*> genGuess <*> genValue ] ++
        [ Guess w   <$> genGuess  <*> genGuess <*> choose (1, val) | Just w <- [tok], val > 0 ] ++
        [ GiveToken <$> genWallet                                  | tok /= Nothing ]
        where
            tok = s ^. modelState . hasToken
            val = s ^. modelState . gameValue

    -- The 'precondition' says when a particular command is allowed.
    precondition s cmd = case cmd of
            Lock _ _ v    -> v > 0 && tok == Nothing
            Guess w _ _ v -> v <= val && tok == Just w
            GiveToken w   -> tok /= Nothing
        where
            tok = s ^. modelState . hasToken
            val = s ^. modelState . gameValue

    shrinkCommand _s (Lock w secret val) =
        [Lock w' secret val | w' <- shrinkWallet w] ++
        [Lock w secret val' | val' <- shrink val]
    shrinkCommand _s (GiveToken w) =
        [GiveToken w' | w' <- shrinkWallet w]
    shrinkCommand _s (Guess w old new val) =
        [Guess w' old new val | w' <- shrinkWallet w] ++
        [Guess w old new val' | val' <- shrink val]

    monitoring _ _ = id

handleSpec :: [HandleSpec GameModel]
handleSpec = [ HandleSpec (WalletKey w) w G.contract | w <- wallets ]

-- | The main property. 'propRunScript_' checks that balances match the model after each test.
prop_Game :: Script GameModel -> Property
prop_Game script = propRunScript_ handleSpec script

propGame' :: LogLevel -> Script GameModel -> Property
propGame' l s = propRunScriptWithOptions
                    (set minLogLevel l defaultCheckOptions)
                    handleSpec
                    (\ _ -> pure True)
                    (\ _ _ -> pure ())
                    s
                    (\ _ -> pure ())

wallets :: [Wallet]
wallets = [w1, w2, w3]

genWallet :: Gen Wallet
genWallet = QC.elements wallets

shrinkWallet :: Wallet -> [Wallet]
shrinkWallet w = [w' | w' <- wallets, w' < w]

genGuess :: Gen String
genGuess = QC.elements ["hello", "secret", "hunter2", "*******"]

genValue :: Gen Integer
genValue = getPositive <$> arbitrary

delay :: Int -> EmulatorTrace ()
delay n = void $ waitNSlots (fromIntegral n)

-- Dynamic Logic ----------------------------------------------------------

prop_UnitTest :: Property
prop_UnitTest = withMaxSuccess 1 $ forAllDL unitTest prop_Game

unitTest :: DL GameModel ()
unitTest = do
    val <- forAllQ $ chooseQ (5, 20)
    action $ Lock w1 "hello" val
    action $ GiveToken w2
    action $ Guess w2 "hello" "new secret" 3

unitTest2 :: DL GameModel ()
unitTest2 = do
    unitTest
    action $ GiveToken w3
    action $ Guess w3 "new secret" "hello" 4

unitTestFail :: DL GameModel ()
unitTestFail = do
    action $ Lock w1 "hello" 8
    action $ GiveToken w2
    action $ Guess w2 "hola" "new secret" 3

noLockedFunds :: DL GameModel ()
noLockedFunds = do
    anyActions_
    w <- forAllQ $ elementsQ wallets
    secret <- viewModelState currentSecret
    val    <- viewModelState gameValue
    when (val > 0) $ do
        action $ GiveToken w
        action $ Guess w secret secret val
    assertModel "Locked funds should be zero" $ isZero . lockedFunds

-- | Check that we can always get the money out of the guessing game (by guessing correctly).
prop_NoLockedFunds :: Property
prop_NoLockedFunds = forAllDL noLockedFunds prop_Game

-- * Unit tests

tests :: TestTree
tests =
    testGroup "state machine tests"
    [ checkPredicate "run a successful game trace"
        (walletFundsChange w2 (Ada.lovelaceValueOf 3 <> gameTokenVal)
        .&&. valueAtAddress (Scripts.scriptAddress G.scriptInstance) (Ada.lovelaceValueOf 5 ==)
        .&&. walletFundsChange w1 (Ada.lovelaceValueOf (-8)))
        successTrace

    , checkPredicate "run a 2nd successful game trace"
        (walletFundsChange w2 (Ada.lovelaceValueOf 3)
        .&&. valueAtAddress (Scripts.scriptAddress G.scriptInstance) (Ada.lovelaceValueOf 1 ==)
        .&&. walletFundsChange w1 (Ada.lovelaceValueOf (-8))
        .&&. walletFundsChange w3 (Ada.lovelaceValueOf 4 <> gameTokenVal))
        successTrace2

    , checkPredicate "run a failed trace"
        (walletFundsChange w2 gameTokenVal
        .&&. valueAtAddress (Scripts.scriptAddress G.scriptInstance) (Ada.lovelaceValueOf 8 ==)
        .&&. walletFundsChange w1 (Ada.lovelaceValueOf (-8)))
        failTrace

    , Lib.goldenPir "test/Spec/gameStateMachine.pir" $$(PlutusTx.compile [|| mkValidator ||])

    , HUnit.testCase "script size is reasonable"
        (Lib.reasonable (Scripts.validatorScript G.scriptInstance) 49000)

    ]

initialVal :: Value
initialVal = Ada.adaValueOf 10

w1 :: Wallet
w1 = Wallet 1

w2 :: Wallet
w2 = Wallet 2

w3 :: Wallet
w3 = Wallet 3

-- | Wallet 1 locks some funds, transfers the token to wallet 2
--   which then makes a correct guess and locks the remaining
--   funds with a new secret
successTrace :: EmulatorTrace ()
successTrace = do
    hdl <- Trace.activateContractWallet w1 G.contract
    Trace.callEndpoint @"lock" hdl LockArgs{lockArgsSecret="hello", lockArgsValue= Ada.lovelaceValueOf 8}
    _ <- Trace.waitNSlots 2
    _ <- Trace.payToWallet w1 w2 gameTokenVal
    _ <- Trace.waitNSlots 1
    hdl2 <- Trace.activateContractWallet w2 G.contract
    Trace.callEndpoint @"guess" hdl2 GuessArgs{guessArgsOldSecret="hello", guessArgsNewSecret="new secret", guessArgsValueTakenOut=Ada.lovelaceValueOf 3}
    void $ Trace.waitNSlots 1

-- | Run 'successTrace', then wallet 2 transfers the token to wallet 3, which
--   makes another correct guess
successTrace2 :: EmulatorTrace ()
successTrace2 = do
    successTrace
    _ <- Trace.payToWallet w2 w3 gameTokenVal
    _ <- Trace.waitNSlots 1
    hdl3 <- Trace.activateContractWallet w3 G.contract
    Trace.callEndpoint @"guess" hdl3 GuessArgs{guessArgsOldSecret="new secret", guessArgsNewSecret="hello", guessArgsValueTakenOut=Ada.lovelaceValueOf 4}
    void $ Trace.waitNSlots 1


-- | Wallet 1 locks some funds, transfers the token to wallet 2
--   which then makes a wrong guess
failTrace :: EmulatorTrace ()
failTrace = do
    hdl <- Trace.activateContractWallet w1 G.contract
    Trace.callEndpoint @"lock" hdl LockArgs{lockArgsSecret="hello", lockArgsValue= Ada.lovelaceValueOf 8}
    _ <- Trace.waitNSlots 2
    _ <- Trace.payToWallet w1 w2 gameTokenVal
    _ <- Trace.waitNSlots 1
    hdl2 <- Trace.activateContractWallet w2 G.contract
    _ <- Trace.callEndpoint @"guess" hdl2 GuessArgs{guessArgsOldSecret="hola", guessArgsNewSecret="new secret", guessArgsValueTakenOut=Ada.lovelaceValueOf 3}
    void $ Trace.waitNSlots 1

gameTokenVal :: Value
gameTokenVal =
    let sym = Scripts.monetaryPolicyHash G.scriptInstance
    in G.token sym "guess"
