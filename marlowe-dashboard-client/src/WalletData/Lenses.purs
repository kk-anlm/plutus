module WalletData.Lenses
  ( _walletLibrary
  , _cardSection
  , _walletId
  , _remoteWalletInfo
  , _walletNickname
  , _companionAppId
  , _marloweAppId
  , _walletInfo
  , _assets
  , _previousCompanionAppState
  , _wallet
  , _pubKey
  , _pubKeyHash
  ) where

import Prelude
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (Assets, MarloweData, MarloweParams, PubKey)
import Types (WebData)
import WalletData.Types (CardSection, PubKeyHash, State, Wallet, WalletInfo, WalletLibrary, WalletDetails)

_walletLibrary :: Lens' State WalletLibrary
_walletLibrary = prop (SProxy :: SProxy "walletLibrary")

_cardSection :: Lens' State CardSection
_cardSection = prop (SProxy :: SProxy "cardSection")

_walletNickname :: forall r a. Lens' { walletNickname :: a | r } a
_walletNickname = prop (SProxy :: SProxy "walletNickname")

_walletId :: forall r a. Lens' { walletId :: a | r } a
_walletId = prop (SProxy :: SProxy "walletId")

_remoteWalletInfo :: Lens' State (WebData WalletInfo)
_remoteWalletInfo = prop (SProxy :: SProxy "remoteWalletInfo")

------------------------------------------------------------
_companionAppId :: Lens' WalletDetails PlutusAppId
_companionAppId = prop (SProxy :: SProxy "companionAppId")

_marloweAppId :: Lens' WalletDetails PlutusAppId
_marloweAppId = prop (SProxy :: SProxy "marloweAppId")

_walletInfo :: Lens' WalletDetails WalletInfo
_walletInfo = prop (SProxy :: SProxy "walletInfo")

_assets :: Lens' WalletDetails Assets
_assets = prop (SProxy :: SProxy "assets")

_previousCompanionAppState :: Lens' WalletDetails (Maybe (Map MarloweParams MarloweData))
_previousCompanionAppState = prop (SProxy :: SProxy "previousCompanionAppState")

------------------------------------------------------------
_wallet :: Lens' WalletInfo Wallet
_wallet = _Newtype <<< prop (SProxy :: SProxy "wallet")

_pubKey :: Lens' WalletInfo PubKey
_pubKey = _Newtype <<< prop (SProxy :: SProxy "pubKey")

_pubKeyHash :: Lens' WalletInfo PubKeyHash
_pubKeyHash = _Newtype <<< prop (SProxy :: SProxy "pubKeyHash")
