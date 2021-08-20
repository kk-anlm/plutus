{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module GameSimulations where

import           Game                  (GuessParams (GuessParams), LockParams (LockParams), amount, guessWord,
                                        registeredKnownCurrencies, secretWord)
import qualified Ledger.Ada            as Ada
import           Playground.Types      (ContractCall (AddBlocks), Simulation (Simulation), SimulatorAction,
                                        simulationActions, simulationId, simulationName, simulationWallets)
import           SimulationUtils       (callEndpoint, simulatorWallet)
import           Wallet.Emulator.Types (Wallet, knownWallet)

simulations :: [Simulation]
simulations = [basicGame, badGuess]
  where
    wallet1 = knownWallet 1
    wallet2 = knownWallet 2
    wallet3 = knownWallet 3
    basicGame =
        Simulation
            { simulationName = "Basic Game"
            , simulationId = 1
            , simulationWallets = simulatorWallet registeredKnownCurrencies 100_000_000 <$> [wallet1, wallet2]
            , simulationActions =
                  [ lock wallet1 "Plutus" 50_000_000
                  , AddBlocks 1
                  , guess wallet2 "Plutus"
                  , AddBlocks 1
                  ]
            }
    badGuess =
        Simulation
            { simulationName = "One Bad Guess"
            , simulationId = 2
            , simulationWallets = simulatorWallet registeredKnownCurrencies 100_000_000 <$> [wallet1, wallet2, wallet3]
            , simulationActions =
                  [ lock wallet1 "Plutus" 50_000_000
                  , AddBlocks 1
                  , guess wallet2 "Marlowe"
                  , AddBlocks 1
                  , guess wallet3 "Plutus"
                  , AddBlocks 1
                  ]
            }

lock :: Wallet -> String -> Integer -> SimulatorAction
lock caller secretWord balance =
    callEndpoint
        caller
        "lock"
        LockParams {secretWord, amount = Ada.lovelaceValueOf balance}

guess :: Wallet -> String -> SimulatorAction
guess caller guessWord = callEndpoint caller "guess" (GuessParams {guessWord})
