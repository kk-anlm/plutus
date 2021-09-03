module Welcome.Lenses
  ( _card
  , _cardOpen
  , _walletLibrary
  , _walletNicknameOrIdInput
  , _remoteWalletDetails
  , _enteringDashboardState
  , module WDL
  ) where

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import InputField.Types (State) as InputField
import Types (WebData)
import WalletData.Types (WalletDetails, WalletLibrary)
import Welcome.Types (Card, State, WalletNicknameOrIdError)
import WalletData.Lenses
  ( _walletNickname
  , _walletNicknameError
  , _walletId
  , _walletIdError
  )
  as WDL

_card :: Lens' State (Maybe Card)
_card = prop (SProxy :: SProxy "card")

_cardOpen :: Lens' State Boolean
_cardOpen = prop (SProxy :: SProxy "cardOpen")

_walletLibrary :: Lens' State WalletLibrary
_walletLibrary = prop (SProxy :: SProxy "walletLibrary")

_walletNicknameOrIdInput :: Lens' State (InputField.State WalletNicknameOrIdError)
_walletNicknameOrIdInput = prop (SProxy :: SProxy "walletNicknameOrIdInput")

_remoteWalletDetails :: Lens' State (WebData WalletDetails)
_remoteWalletDetails = prop (SProxy :: SProxy "remoteWalletDetails")

_enteringDashboardState :: Lens' State Boolean
_enteringDashboardState = prop (SProxy :: SProxy "enteringDashboardState")
