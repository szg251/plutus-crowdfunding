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
  CloseParams (..),
  rewardTokenCurSymbol,
  rewardTokenName,

  -- * Scripts
  crowdfundingValidator,

  -- * Address
  crowdfundingAddress,
  validateCrowdfunding,

  -- * Traces
  supportTrace,
  closeTrace,
  myTrace,
  runMyTrace,
) where

import Control.Monad (void)
import qualified Control.Monad.Freer.Extras.Log as Extras
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Char8 as C
import Data.Fixed (Micro)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Void (Void)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Ledger (Address, CurrencySymbol, Datum (Datum), POSIXTime, PubKeyHash, ScriptContext, Slot, TokenName, TxOutTx, Validator, Value)
import qualified Ledger
import qualified Ledger.Ada as Ada
import Ledger.AddressMap (UtxoMap)
import qualified Ledger.Constraints as Constraints
import qualified Ledger.Interval as Interval
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import Plutus.Contract
import Plutus.Contract.Schema ()
import Plutus.Contract.Types
import Plutus.Trace.Emulator (EmulatorTrace, observableState, runEmulatorTraceIO)
import qualified Plutus.Trace.Emulator as Trace
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..))
import qualified Relude
import qualified Relude.Extra.Tuple as Tuple
import Schema (ToArgument, ToSchema)
import Wallet.Emulator (Wallet (..), walletPubKey)
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

data CrowdfundingAction = Collect | Return
  deriving (Relude.Show)

PlutusTx.unstableMakeIsData ''CrowdfundingAction

newtype SupportDatum = SupportDatum {refundAddress :: PubKeyHash}
  deriving (Relude.Show)

PlutusTx.unstableMakeIsData ''SupportDatum

data Crowdfunding
instance Scripts.ValidatorTypes Crowdfunding where
  type RedeemerType Crowdfunding = CrowdfundingAction
  type DatumType Crowdfunding = SupportDatum

eitherValidator :: Either BuiltinString a -> Bool
eitherValidator (Left e) = traceIfFalse e False
eitherValidator (Right _) = True

{-# INLINEABLE mkRewardTokenPolicy #-}
mkRewardTokenPolicy :: CrowdfundingParams -> Address -> () -> ScriptContext -> Bool
mkRewardTokenPolicy cfParams@CrowdfundingParams{crowdfundingOwnerPkh, crowdfundingDeadline} scriptAddress () ctx =
  traceIfFalse "deadline passed" isBeforeDeadline
    && traceIfFalse "reward token to support amount ratio is invalid" isRewardRatioOk
 where
  isBeforeDeadline = Interval.to crowdfundingDeadline `Interval.contains` validRange

  txInfo = Ledger.scriptContextTxInfo ctx
  validRange = Ledger.txInfoValidRange txInfo

  isRewardRatioOk = rewardAmt * crowdfundingRewardRatio cfParams <= supportAmt
  ownAddress = Ledger.txOutAddress . Ledger.txInInfoResolved <$> Ledger.findOwnInput ctx

  supportTxOuts =
    filter (\o -> Ledger.txOutAddress o == scriptAddress) $ Ledger.txInfoOutputs txInfo
  adaAssetClass = Value.assetClass Ada.adaSymbol Ada.adaToken
  supportAmt =
    sum $ flip Value.assetClassValueOf adaAssetClass . Ledger.txOutValue <$> supportTxOuts

  ownAssetClass = Value.assetClass (Ledger.ownCurrencySymbol ctx) rewardTokenName
  rewardAmt =
    Value.assetClassValueOf (Ledger.txInfoMint txInfo) ownAssetClass

rewardTokenPolicy :: CrowdfundingParams -> Scripts.MintingPolicy
rewardTokenPolicy cfParams =
  Ledger.mkMintingPolicyScript $
    $$(PlutusTx.compile [||(\p a -> Scripts.wrapMintingPolicy $ mkRewardTokenPolicy p a)||])
      `PlutusTx.applyCode` PlutusTx.liftCode cfParams
      `PlutusTx.applyCode` PlutusTx.liftCode (crowdfundingAddress cfParams)

rewardTokenCurSymbol :: CrowdfundingParams -> CurrencySymbol
rewardTokenCurSymbol = Ledger.scriptCurrencySymbol . rewardTokenPolicy

{-# INLINEABLE rewardTokenName #-}
rewardTokenName :: TokenName
rewardTokenName = Value.TokenName "reward token"

-- | The validation function (Params -> DataValue -> RedeemerValue -> ScriptContext -> Bool)
{-# INLINEABLE validateCrowdfunding #-}
validateCrowdfunding :: CrowdfundingParams -> SupportDatum -> CrowdfundingAction -> ScriptContext -> Bool
validateCrowdfunding CrowdfundingParams{crowdfundingDeadline, crowdfundingTargetAmount, crowdfundingOwnerPkh} _ action ctx =
  case action of
    Collect ->
      traceIfFalse "Must meet the crowdfunding target to collect" isTargetMet
        && traceIfFalse "Must be signed by crowdfunding owner" isSignedByOwner
        && traceIfFalse "Cannot close before the deadline" isAfterDeadline
        && traceIfFalse "Successful crowdfunding assets must be sent to owner" isSentToOwner
    Return ->
      traceIfFalse "Cannot meet the crowdfunding target to get refund" (not isTargetMet)
        && traceIfFalse "Cannot close before the deadline" isAfterDeadline
        && traceIfFalse "Each supporter must get a refund" isSentToSupporters
 where
  isTargetMet = collectedAmt >= crowdfundingTargetAmount
  isSignedByOwner = Ledger.txSignedBy txInfo crowdfundingOwnerPkh
  isAfterDeadline = Interval.from crowdfundingDeadline `Interval.contains` validRange
  isSentToOwner =
    all
      (\o -> Ledger.txOutAddress o == Ledger.pubKeyHashAddress crowdfundingOwnerPkh)
      txOuts
  isSentToSupporters =
    all
      ( \i ->
          any (\o -> Just (Ledger.txOutAddress o) == fmap Ledger.pubKeyHashAddress (findPkh i)) txOuts
      )
      txIns

  collectedAmt =
    sum [Value.valueOf (Ledger.txOutValue i) Ada.adaSymbol Ada.adaToken | i <- txIns]

  txInfo = Ledger.scriptContextTxInfo ctx
  txOuts = Ledger.txInfoOutputs txInfo
  txIns =
    filter (isJust . findPkh) $ map Ledger.txInInfoResolved $ Ledger.txInfoInputs txInfo
  validRange = Ledger.txInfoValidRange txInfo

  findPkh txout = do
    datumH <- Ledger.txOutDatum txout
    Datum datum <- Ledger.findDatum datumH txInfo
    SupportDatum{refundAddress} <- PlutusTx.fromBuiltinData datum
    Just refundAddress

-- | The validator script of the crowdfunding.
crowdfundingValidator :: CrowdfundingParams -> Validator
crowdfundingValidator = Scripts.validatorScript . crowdfundingInstance

crowdfundingInstance :: CrowdfundingParams -> Scripts.TypedValidator Crowdfunding
crowdfundingInstance cfParams =
  Scripts.mkTypedValidator @Crowdfunding
    ( $$(PlutusTx.compile [||validateCrowdfunding||])
        `PlutusTx.applyCode` PlutusTx.liftCode cfParams
    )
    $$(PlutusTx.compile [||wrap||])
 where
  wrap = Scripts.wrapValidator @SupportDatum @CrowdfundingAction

crowdfundingAddress :: CrowdfundingParams -> Address
crowdfundingAddress = Ledger.scriptAddress . crowdfundingValidator

type CrowdfundingSchema =
  Endpoint "support" SupportParams
    .\/ Endpoint "close" CloseParams

data SupportParams = SupportParams
  { spCrowdfundingParams :: CrowdfundingParams
  , spAmount :: Integer
  }
  deriving stock (Relude.Eq, Relude.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype CloseParams = CloseParams {cpCrowdfundingParams :: CrowdfundingParams}
  deriving stock (Relude.Eq, Relude.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

crowdfunding :: (AsContractError e, ToJSON e) => Contract () CrowdfundingSchema e ()
crowdfunding =
  selectList [support, close] >> crowdfunding

support :: (AsContractError e) => Promise () CrowdfundingSchema e ()
support = endpoint @"support" @SupportParams $ \(SupportParams cfParams amt) -> do
  logInfo @Relude.String $ "Pay " <> Relude.show amt <> " to the script"

  pkh <- Ledger.pubKeyHash <$> ownPubKey

  let rewardAmt = amt `Relude.quot` crowdfundingRewardRatio cfParams
      rewardTokens = Value.singleton (rewardTokenCurSymbol cfParams) rewardTokenName rewardAmt

      lookups =
        Constraints.typedValidatorLookups (crowdfundingInstance cfParams)
          <> Constraints.mintingPolicy (rewardTokenPolicy cfParams)

      val = Ada.lovelaceValueOf amt

      datum = SupportDatum pkh

      tx =
        Constraints.mustPayToTheScript datum val
          <> Constraints.mustMintValue rewardTokens
          <> Constraints.mustValidateIn (Interval.to (crowdfundingDeadline cfParams))

  void $ submitTxConstraintsWith lookups tx

close :: (AsContractError e) => Promise () CrowdfundingSchema e ()
close = endpoint @"close" @CloseParams $ \(CloseParams cfParams) -> do
  logInfo @Relude.String "Closing crowdfunding"

  utxos <- utxoAt (crowdfundingAddress cfParams)

  let collectedVal = sum $ Ledger.txOutValue . Ledger.txOutTxOut <$> Map.elems utxos
      collectedAmt = Value.valueOf collectedVal Ada.adaSymbol Ada.adaToken

  if collectedAmt >= crowdfundingTargetAmount cfParams
    then collectFunds cfParams utxos
    else returnFunds cfParams utxos

-- | Collecting funds to owner account after a successful crowdfunding campaign
collectFunds ::
  (AsContractError e) =>
  CrowdfundingParams ->
  UtxoMap ->
  Contract () CrowdfundingSchema e ()
collectFunds cfParams utxos = do
  logInfo @Relude.String "Crowdfunding successful, collecting funds"
  let lookups =
        Constraints.unspentOutputs utxos
          <> Constraints.otherScript (crowdfundingValidator cfParams)

      redeemer = Ledger.Redeemer (PlutusTx.toBuiltinData Collect)
      tx =
        mconcat [Constraints.mustSpendScriptOutput utxo redeemer | utxo <- Map.keys utxos]
          <> Constraints.mustBeSignedBy (crowdfundingOwnerPkh cfParams)
          <> Constraints.mustValidateIn (Interval.from (crowdfundingDeadline cfParams))

  void $ submitTxConstraintsWith @Crowdfunding lookups tx

-- | Returning funds to the original owners after a failed crowdfunding campaign
returnFunds ::
  (AsContractError e) =>
  CrowdfundingParams ->
  UtxoMap ->
  Contract () CrowdfundingSchema e ()
returnFunds cfParams utxos = do
  logInfo @Relude.String "Crowdfunding unsuccessful, returning funds"
  let supporters = map findSupporter $ Map.elems utxos

  let lookups =
        Constraints.unspentOutputs utxos
          <> Constraints.otherScript (crowdfundingValidator cfParams)

      redeemer = Ledger.Redeemer (PlutusTx.toBuiltinData Return)
      tx =
        mconcat [Constraints.mustSpendScriptOutput utxo redeemer | utxo <- Map.keys utxos]
          <> mconcat [Constraints.mustPayToPubKey pkh v | Just (pkh, v) <- supporters]
          <> Constraints.mustValidateIn (Interval.from (crowdfundingDeadline cfParams))

  void $ submitTxConstraintsWith @Crowdfunding lookups tx

findSupporter :: TxOutTx -> Maybe (PubKeyHash, Value)
findSupporter =
  discardUnknown
    . Relude.bimap getPkh getValue
    . Tuple.dup
 where
  getPkh txOutTx = do
    let tx = Ledger.txOutTxTx txOutTx
    datumH <- Ledger.txOutDatum $ Ledger.txOutTxOut txOutTx
    Datum datum <- Ledger.lookupDatum tx datumH
    SupportDatum{refundAddress} <- PlutusTx.fromBuiltinData datum
    Just refundAddress

  getValue = Ledger.txOutValue . Ledger.txOutTxOut

  discardUnknown (Just pkh, v) = Just (pkh, v)
  discardUnknown _ = Nothing

supportTrace :: Wallet -> CrowdfundingParams -> Integer -> EmulatorTrace ()
supportTrace wallet cfParams amount = do
  hdl <- Trace.activateContractWallet wallet (crowdfunding @ContractError)
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"support" hdl (SupportParams cfParams amount)
  void $ Trace.waitNSlots 1

closeTrace :: Wallet -> CrowdfundingParams -> EmulatorTrace ()
closeTrace wallet cfParams = do
  hdl <- Trace.activateContractWallet wallet (crowdfunding @ContractError)
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"close" hdl (CloseParams cfParams)
  void $ Trace.waitNSlots 1

myTrace :: Wallet -> Wallet -> Wallet -> CrowdfundingParams -> EmulatorTrace ()
myTrace w1 w2 w3 cfParams = do
  supportTrace w2 cfParams 500_000
  supportTrace w3 cfParams 480_000
  void $ Trace.waitUntilTime (crowdfundingDeadline cfParams)
  closeTrace w1 cfParams

runMyTrace :: Relude.IO ()
runMyTrace = do
  let deadline = 1596059100000
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
  runEmulatorTraceIO $ myTrace w1 w2 w3 cfParams
