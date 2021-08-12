{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Spec.Model (
  tests,
  test,
) where

import Control.Lens hiding (elements)
import Control.Monad hiding (fmap)
import Data.Default (Default (def))
import Ledger (POSIXTime)
import qualified Ledger
import Ledger.Ada as Ada
import qualified Ledger.TimeSlot as TimeSlot
import qualified Ledger.Value as Value
import Plutus.Contract (AsContractError, ContractError)
import Plutus.Contract.Test (defaultCheckOptions, maxSlot)
import Plutus.Contract.Test.ContractModel
import Plutus.Contracts.Crowdfunding
import qualified Plutus.Trace.Emulator as Trace
import Relude
import qualified Relude.Extra.Map as Map
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Wallet.Emulator (Wallet (..), walletPubKey)

data CFState = CFState
  { _cfmCFParams :: CrowdfundingParams
  , _cfmSupporters :: Map Wallet Integer
  }
  deriving (Show)

makeLenses ''CFState

newtype CFModel = CFModel {_cfModel :: Map Wallet CFState}
  deriving (Show)

makeLenses ''CFModel

tests :: TestTree
tests = testProperty "crowdfunding model" prop_CF

instance ContractModel CFModel where
  data Action CFModel
    = Start Wallet POSIXTime Integer Integer -- Start wallet deadline targetAmount rewardRatio
    | Support Wallet Wallet Integer --  Support fromWallet toCrowdfundingOfWallet amount
    | Close Wallet Wallet
    deriving (Show, Eq)

  data ContractInstanceKey CFModel w s e where
    CrowdfundingKey :: AsContractError e => Wallet -> ContractInstanceKey CFModel () CrowdfundingSchema e

  arbitraryAction _ =
    oneof
      [ Start <$> genWallet <*> genDeadline <*> genAmount <*> genRewardRatio
      , Support <$> genWallet <*> genWallet <*> genAmount
      , Close <$> genWallet <*> genWallet
      ]

  initialState = CFModel mempty

  nextState (Start w deadline targetAmount rewardRatio) = do
    (cfModel . at w)
      $= Just
        ( CFState
            ( CrowdfundingParams
                { crowdfundingDeadline = deadline
                , crowdfundingTargetAmount = targetAmount
                , crowdfundingOwnerPkh = Ledger.pubKeyHash $ walletPubKey w
                , crowdfundingRewardRatio = rewardRatio
                }
            )
            mempty
        )
  nextState (Support w1 w2 amt) = do
    when (amt > 0) $ do
      maybeCfParams <- getCFParams w2
      currentTime <- getCurrentTime
      case maybeCfParams of
        Nothing -> return ()
        Just cfParams -> do
          when (currentTime < crowdfundingDeadline cfParams) $ do
            let rewardTokens =
                  Value.singleton
                    (rewardTokenCurSymbol cfParams)
                    rewardTokenName
                    (amt `quot` crowdfundingRewardRatio cfParams)

            withdraw w1 (Ada.lovelaceValueOf amt)
            mint rewardTokens
            deposit w1 rewardTokens
            (cfModel . ix w2 . cfmSupporters . at w1) $~ (\prev -> Just $ amt + fromMaybe 0 prev)
    wait 2
  nextState (Close w1 w2) = do
    s <- getModelState
    maybeCfParams <- getCFParams w2
    currentTime <- getCurrentTime
    case maybeCfParams of
      Nothing -> return ()
      Just cfParams ->
        when (currentTime >= crowdfundingDeadline cfParams) $ do
          let supporters = s ^. contractState . cfModel . ix w2 . cfmSupporters
          let crowdfundingResult = sum $ Map.elems supporters
          if crowdfundingTargetAmount cfParams >= crowdfundingResult
            then when (w1 == w2) $ deposit w1 (Ada.lovelaceValueOf crowdfundingResult)
            else do
              let supporters = s ^. contractState . cfModel . ix w2 . cfmSupporters
              mapM_ (\(w, amt) -> deposit w (Ada.lovelaceValueOf amt)) $ Map.toPairs supporters
              (cfModel . ix w2 . cfmSupporters . at w1) $= Nothing
    wait 2

  perform _ _ Start{} = return ()
  perform h s (Support w1 w2 amount) = do
    case getCFParams' s w2 of
      Nothing -> return ()
      Just cfParams -> do
        Trace.callEndpoint @"support"
          (h $ CrowdfundingKey @ContractError w1)
          (SupportParams cfParams amount)
        void $ Trace.waitNSlots 2
  perform h s (Close w1 w2) =
    case getCFParams' s w2 of
      Nothing -> return ()
      Just cfParams -> do
        Trace.callEndpoint @"close" (h $ CrowdfundingKey @ContractError w1) (CloseParams cfParams)
        void $ Trace.waitNSlots 2

  precondition s (Start w _ _ _) = isNothing $ getCFParams' s w
  precondition s (Support _ w2 _) = isJust $ getCFParams' s w2
  precondition s (Close w1 w2) = isJust (getCFParams' s w2) && w1 == w2

deriving instance Eq (ContractInstanceKey CFModel w s e)
deriving instance Show (ContractInstanceKey CFModel w s e)

getCFParams' :: ModelState CFModel -> Wallet -> Maybe CrowdfundingParams
getCFParams' s w = s ^. contractState . cfModel . at w & fmap (^. cfmCFParams)

getCFParams :: Wallet -> Spec CFModel (Maybe CrowdfundingParams)
getCFParams w = do
  s <- getModelState
  return $ getCFParams' s w

getCurrentTime :: Spec CFModel POSIXTime
getCurrentTime = do
  s <- viewModelState currentSlot
  return (TimeSlot.slotToEndPOSIXTime def s)

wallets :: [Wallet]
wallets = [Wallet 1, Wallet 2, Wallet 3]

instanceSpec :: [ContractInstanceSpec CFModel]
instanceSpec =
  [ContractInstanceSpec (CrowdfundingKey w) w (crowdfunding @ContractError) | w <- wallets]

genWallet :: Gen Wallet
genWallet = elements wallets

genRewardRatio :: Gen Integer
genRewardRatio = (100 *) . getPositive <$> arbitrary

genAmount :: Gen Integer
genAmount = (1000 *) . getPositive <$> arbitrary

genDeadline :: Gen POSIXTime
genDeadline = (\x -> fromInteger $ 1596059091000 + (10_000 * x)) <$> chooseInteger (0, 10)

prop_CF :: Actions CFModel -> Property
prop_CF =
  withMaxSuccess 100
    . propRunActionsWithOptions
      (defaultCheckOptions & maxSlot .~ 2000)
      instanceSpec
      (const $ pure True)

test :: IO ()
test = quickCheck prop_CF
