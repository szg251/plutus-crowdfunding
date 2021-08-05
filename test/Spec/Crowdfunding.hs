{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Spec.Crowdfunding (
  tests,
) where

import Control.Monad (void)
import Ledger (Value, pubKeyHash)
import qualified Ledger.Ada as Ada
import Ledger.Value as Value
import Plutus.Contract (Contract, ContractError)
import Plutus.Contract.Test
import Plutus.Contracts.Crowdfunding
import Plutus.Trace.Emulator (ContractInstanceTag, waitNSlots, waitUntilTime)
import qualified Plutus.Trace.Emulator as Trace
import qualified PlutusTx
import Relude hiding (not)
import Test.Tasty
import qualified Test.Tasty.HUnit as HUnit

w1, w2 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3

t1, t2 :: ContractInstanceTag
t1 = Trace.walletInstanceTag w1
t2 = Trace.walletInstanceTag w2

theContract :: Contract () CrowdfundingSchema ContractError ()
theContract = crowdfunding

cfParams :: CrowdfundingParams
cfParams =
  CrowdfundingParams
    { crowdfundingDeadline = 1600000000000
    , crowdfundingTargetAmount = 2_000_000
    , crowdfundingOwnerPkh = pubKeyHash $ walletPubKey w1
    , crowdfundingRewardRatio = 1_000
    }

rewardAssetClass :: AssetClass
rewardAssetClass = AssetClass (rewardTokenCurSymbol cfParams, TokenName "reward token")

tests :: TestTree
tests =
  testGroup
    "crowdfunding"
    [ checkPredicate
        "support endpoint submits a transaction"
        (anyTx theContract t2)
        $ do
          hdl <- Trace.activateContractWallet w2 theContract
          Trace.callEndpoint @"support" hdl (SupportParams cfParams 1_000_000)
    , checkPredicate
        "'support' will send the specified amount to the script address"
        (walletFundsChange w2 (Ada.adaValueOf (-1) <> Value.assetClassValue rewardAssetClass 1000))
        $ supportTrace w2 cfParams 1_000_000
    , checkPredicate
        "'support' will disallow funding after deadline"
        (not $ anyTx theContract t2)
        $ do
          void $ waitUntilTime 1600000000000
          supportTrace w2 cfParams 1_000_000
    , checkPredicate
        "'close' will fail before deadline"
        (not $ anyTx theContract t1)
        $ do
          supportTrace w2 cfParams 2_000_000
          closeTrace w1 cfParams
    , -- , checkPredicate
      --     "'close' will collect funds to owner address, if crowdfunding is successful"
      --     (walletFundsChange w1 (Ada.adaValueOf 2))
      --     $ do
      --       supportTrace w2 cfParams 1_000_000
      --       supportTrace w3 cfParams 1_00_000
      --       waitUntilTime 1600000000000
      --       closeTrace w1 cfParams
      -- , checkPredicate
      --     "'close' will return funds to supporters, if crowdfunding is unsuccessful"
      --     (walletFundsChange w2 (Ada.adaValueOf 1) .&&. walletFundsChange w3 (Ada.adaValueOf 0.8)
      --     $ do
      --       supportTrace w2 cfParams 1_000_000
      --       supportTrace w3 cfParams 800_000
      --       void $ waitUntilTime 1600000000000
      --       closeTrace w1 cfParams
      -- , goldenPir
      --   "test/Spec/crowdfunding.pir"
      --   ( $$(PlutusTx.compile [||validateCrowdfunding||])
      --       `PlutusTx.applyCode` PlutusTx.liftCode cfParams
      --   )
      HUnit.testCase "script size is reasonable" (reasonable (crowdfundingValidator cfParams) 20000)
    ]
