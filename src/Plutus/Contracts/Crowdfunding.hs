{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Crowdfunding smart contract
module Plutus.Contracts.Crowdfunding (
  support,
  crowdfunding,
  CrowdfundingSchema,
  CrowdfundingParams (..),
  SupportParams (..),
  curSymbol,

  -- * Scripts
  crowdfundingValidator,

  -- * Address
  crowdfundingAddress,
  validateCrowdfunding,

  -- * Traces
  supportTrace,
  myTrace,
) where

import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Fixed (Micro)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Ledger (Address, CurrencySymbol, Datum (Datum), POSIXTime, PubKeyHash, ScriptContext, Slot, TokenName, TxOutTx, Validator, Value)
import qualified Ledger
import qualified Ledger.Ada as Ada
import Ledger.AddressMap (UtxoMap)
import qualified Ledger.Constraints as Constraints
import qualified Ledger.Interval as Interval
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value as Value
import Plutus.Contract
import Plutus.Contract.Schema ()
import Plutus.Contract.Types
import Plutus.Trace.Emulator (EmulatorTrace, observableState, runEmulatorTraceIO)
import qualified Plutus.Trace.Emulator as Trace
import qualified PlutusTx
import Schema (ToArgument, ToSchema)
import Wallet.Emulator (Wallet (..), walletPubKey)

import qualified Control.Monad.Freer.Extras.Log as Extras
import qualified Data.ByteString.Char8 as C
import Data.Maybe (catMaybes)
import Data.Void (Void)
import PlutusTx.Prelude hiding (Semigroup (..))
import qualified Relude
import Prelude (Semigroup (..))
import qualified Prelude as Haskell

data CrowdfundingParams = CrowdfundingParams
  { crowdfundingDeadline :: !POSIXTime
  , crowdfundingTargetAmount :: !Integer
  , crowdfundingOwnerPkh :: !PubKeyHash
  , crowdfundingRewardRatio :: !Integer
  }
  deriving stock (Relude.Eq, Relude.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

PlutusTx.makeLift ''CrowdfundingParams

type CrowdfundingSchema =
  Endpoint "support" SupportParams
    .\/ Endpoint "close" ()

{-# INLINEABLE mkPolicy #-}
mkPolicy :: CrowdfundingParams -> Address -> () -> ScriptContext -> Bool
mkPolicy cfParams@CrowdfundingParams{crowdfundingOwnerPkh, crowdfundingDeadline} scriptAddress () ctx =
  traceIfFalse "deadline passed" isBeforeDeadline
    && traceIfFalse "reward token to support amount ratio is invalid" isRewardRatioOk
 where
  isBeforeDeadline = Interval.to crowdfundingDeadline `Interval.contains` validRange

  txInfo = Ledger.scriptContextTxInfo ctx
  validRange = Ledger.txInfoValidRange txInfo

  isRewardRatioOk = rewardAmt * crowdfundingRewardRatio cfParams <= supportAmt

  supportTxOuts =
    filter (\o -> Ledger.txOutAddress o == scriptAddress) $ Ledger.txInfoOutputs txInfo
  rewardTxOuts =
    filter (\o -> Ledger.txOutAddress o /= scriptAddress) $ Ledger.txInfoOutputs txInfo

  adaAssetClass = AssetClass (Ada.adaSymbol, Ada.adaToken)
  supportAmt =
    sum $
      flip Value.assetClassValueOf adaAssetClass . Ledger.txOutValue
        <$> supportTxOuts

  ownAssetClass = AssetClass (Ledger.ownCurrencySymbol ctx, TokenName "reward token")
  rewardAmt =
    sum $
      flip Value.assetClassValueOf ownAssetClass . Ledger.txOutValue
        <$> rewardTxOuts

policy :: CrowdfundingParams -> Scripts.MintingPolicy
policy cfParams =
  Ledger.mkMintingPolicyScript $
    $$(PlutusTx.compile [||(\p a -> Scripts.wrapMintingPolicy $ mkPolicy p a)||])
      `PlutusTx.applyCode` PlutusTx.liftCode cfParams
      `PlutusTx.applyCode` PlutusTx.liftCode (crowdfundingAddress cfParams)

curSymbol :: CrowdfundingParams -> CurrencySymbol
curSymbol = Ledger.scriptCurrencySymbol . policy

-- | The validation function (Params -> DataValue -> RedeemerValue -> ScriptContext -> Bool)
{-# INLINEABLE validateCrowdfunding #-}
validateCrowdfunding :: CrowdfundingParams -> () -> () -> ScriptContext -> Bool
validateCrowdfunding CrowdfundingParams{crowdfundingDeadline} _ action ctx =
  traceIfFalse "Cannot close before the deadline" isAfterDeadline
 where
  txInfo = Ledger.scriptContextTxInfo ctx
  validRange = Ledger.txInfoValidRange txInfo

  isAfterDeadline = Interval.from crowdfundingDeadline `Interval.contains` validRange

-- | The validator script of the crowdfunding.
crowdfundingValidator :: CrowdfundingParams -> Validator
crowdfundingValidator = Scripts.validatorScript . crowdfundingInstance

data Crowdfunding
instance Scripts.ValidatorTypes Crowdfunding where
  type RedeemerType Crowdfunding = ()
  type DatumType Crowdfunding = ()

crowdfundingInstance :: CrowdfundingParams -> Scripts.TypedValidator Crowdfunding
crowdfundingInstance cfParams =
  Scripts.mkTypedValidator @Crowdfunding
    ( $$(PlutusTx.compile [||validateCrowdfunding||])
        `PlutusTx.applyCode` PlutusTx.liftCode cfParams
    )
    $$(PlutusTx.compile [||wrap||])
 where
  wrap = Scripts.wrapValidator @() @()

-- | The address of the crowdfunding (the hash of its validator script)
crowdfundingAddress :: CrowdfundingParams -> Address
crowdfundingAddress = Ledger.scriptAddress . crowdfundingValidator

-- | Parameters for the "lock" endpoint
data SupportParams = SupportParams
  { spCrowdfundingParams :: CrowdfundingParams
  , spAmount :: Integer
  }
  deriving stock (Relude.Eq, Relude.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

crowdfunding :: (AsContractError e, ToJSON e) => Contract () CrowdfundingSchema e ()
crowdfunding = do
  logInfo @Relude.String "Waiting for guess or lock endpoint..."
  selectList [support]

support :: (AsContractError e) => Promise () CrowdfundingSchema e ()
support = endpoint @"support" @SupportParams $ \(SupportParams cfParams amt) -> do
  logInfo @Relude.String $ "Pay " <> Relude.show amt <> " to the script"

  let rewardAmt = amt `Relude.quot` crowdfundingRewardRatio cfParams
      rewardTokens = Value.singleton (curSymbol cfParams) "reward token" rewardAmt

      lookups =
        Constraints.typedValidatorLookups (crowdfundingInstance cfParams)
          <> Constraints.mintingPolicy (policy cfParams)

      val = Ada.lovelaceValueOf amt

      tx =
        Constraints.mustPayToTheScript () val
          <> Constraints.mustMintValue rewardTokens
          <> Constraints.mustValidateIn (Interval.to (crowdfundingDeadline cfParams))

      inst = crowdfundingInstance cfParams
  void $ submitTxConstraintsWith lookups tx

supportTrace :: Wallet -> CrowdfundingParams -> Integer -> EmulatorTrace ()
supportTrace wallet cfParams amount = do
  hdl <- Trace.activateContractWallet wallet (support @ContractError)
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"support" hdl (SupportParams cfParams amount)
  void $ Trace.waitNSlots 1
  void $ Trace.observableState hdl

myTrace :: Relude.IO ()
myTrace = do
  let deadline = 1600000000000
  runEmulatorTraceIO $ do
    let w1 = Wallet 1
    let w2 = Wallet 2
    let w3 = Wallet 3
    let cfParams =
          CrowdfundingParams
            { crowdfundingDeadline = deadline
            , crowdfundingTargetAmount = 1_000_000
            , crowdfundingOwnerPkh = Ledger.pubKeyHash $ walletPubKey w1
            , crowdfundingRewardRatio = 1_000
            }
    supportTrace w2 cfParams 5500
    supportTrace w3 cfParams 1000
