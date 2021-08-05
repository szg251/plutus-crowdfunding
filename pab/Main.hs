{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Monad (void)
import Control.Monad.Freer (Eff, Member, interpret, type (~>))
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Extras.Log (LogMsg)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (
  FromJSON (..),
  Options (..),
  ToJSON (..),
  defaultOptions,
  genericParseJSON,
  genericToJSON,
 )
import Data.Default (Default (def))
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import GHC.Generics (Generic)
import Plutus.Contract (ContractError)
import Plutus.Contracts.Crowdfunding as Crowdfunding
import Plutus.PAB.Effects.Contract (ContractEffect (..))
import Plutus.PAB.Effects.Contract.Builtin (Builtin, HasDefinitions, SomeBuiltin (..), type (.\\))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Plutus.PAB.Monitoring.PABLogMsg (PABMultiAgentMsg)
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator as Simulator
import Plutus.PAB.Types (PABError (..))
import qualified Plutus.PAB.Webserver.Server as PAB.Server
import Wallet.Emulator.Types (Wallet (..))

main :: IO ()
main = void $
  Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin MyContracts) "Starting plutus-crowdfunding PAB webserver. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug
    void $ liftIO getLine

    Simulator.logString @(Builtin MyContracts) "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin MyContracts) b

    shutdown

data MyContracts
  = CrowdfundingContract
  deriving (Eq, Ord, Show, Generic)

instance ToJSON MyContracts where
  toJSON =
    genericToJSON
      defaultOptions
        { tagSingleConstructors = True
        }
instance FromJSON MyContracts where
  parseJSON =
    genericParseJSON
      defaultOptions
        { tagSingleConstructors = True
        }

instance Pretty MyContracts where
  pretty = viaShow

instance HasDefinitions MyContracts where
  getDefinitions = [CrowdfundingContract]
  getSchema = \case
    CrowdfundingContract -> Builtin.endpointsToSchemas @Crowdfunding.CrowdfundingSchema
  getContract = \case
    dfundingContractGameContract -> SomeBuiltin (Crowdfunding.crowdfunding @ContractError)

handlers :: SimulatorEffectHandlers (Builtin MyContracts)
handlers =
  Simulator.mkSimulatorHandlers @(Builtin MyContracts) def def $
    interpret (Builtin.contractHandler Builtin.handleBuiltin)
