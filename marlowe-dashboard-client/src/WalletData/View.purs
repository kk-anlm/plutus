module WalletData.View
  ( walletDataCard
  , walletIdTip
  ) where

import Prelude hiding (div)
import Clipboard (Action(..)) as Clipboard
import Css as Css
import Data.Lens ((^.))
import Data.Map (isEmpty, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Data.UUID (toString) as UUID
import Effect.Aff.Class (class MonadAff)
import Halogen (ComponentHTML)
import Halogen.Css (classNames)
import Halogen.HTML (HTML, a, button, div, div_, h2, h3, h4, label, li, p, span, text, ul_)
import Halogen.HTML.Events.Extra (onClick_)
import Halogen.HTML.Properties (disabled, for)
import Input.Text as TInput
import MainFrame.Types (ChildSlots, dashboardWalletDataIdSlot, dashboardWalletDataNicknameSlot)
import Material.Icons (Icon(..)) as Icon
import Material.Icons (icon_)
import WalletData.Lenses (_cardSection, _companionAppId, _walletLibrary, _walletNickname)
import WalletData.State (parseWalletDetails, parseWalletId, parseWalletNickname)
import WalletData.Types (Action(..), CardSection(..), State, WalletDetails, WalletLibrary)

walletDataCard ::
  forall m.
  MonadAff m =>
  WalletDetails ->
  State ->
  ComponentHTML Action ChildSlots m
walletDataCard currentWallet state =
  let
    walletLibrary = state ^. _walletLibrary

    cardSection = state ^. _cardSection
  in
    div
      [ classNames [ "h-full", "grid", "grid-rows-auto-auto-1fr" ] ]
      [ h2
          [ classNames Css.cardHeader ]
          [ text "Contacts" ]
      , walletDataBreadcrumb cardSection
      , case cardSection of
          Home -> walletLibraryCard walletLibrary
          ViewWallet walletDetails -> walletDetailsCard currentWallet walletDetails
          NewWallet mTokenName -> newWalletCard walletLibrary mTokenName state
      ]

walletDataBreadcrumb :: forall p. CardSection -> HTML p Action
walletDataBreadcrumb cardSection =
  div
    [ classNames [ "overflow-x-auto", "flex", "align-baseline", "px-4", "gap-1", "border-gray", "border-b", "text-xs" ] ] case cardSection of
    Home -> [ activeItem "Home" ]
    ViewWallet walletDetails ->
      [ previousItem "Home" Home
      , arrow
      , activeItem $ walletDetails ^. _walletNickname
      ]
    NewWallet mTokenName ->
      [ previousItem "Home" Home
      , arrow
      , case mTokenName of
          Nothing -> activeItem "New Contact"
          Just tokenName -> activeItem $ "New Contact for " <> tokenName <> " Role"
      ]
  where
  activeItem itemText =
    span
      [ classNames [ "whitespace-nowrap", "py-2.5", "border-black", "border-b-2", "font-semibold" ] ]
      [ text itemText ]

  previousItem itemText stage =
    a
      [ classNames [ "whitespace-nowrap", "py-2.5", "text-purple", "border-transparent", "border-b-2", "hover:border-purple", "font-semibold" ]
      , onClick_ $ SetCardSection stage
      ]
      [ text itemText ]

  arrow = span [ classNames [ "mt-2" ] ] [ icon_ Icon.Next ]

walletLibraryCard :: forall p. WalletLibrary -> HTML p Action
walletLibraryCard walletLibrary =
  div
    [ classNames [ "overflow-y-auto" ] ]
    [ if isEmpty walletLibrary then
        -- If you're here, the walletLibrary can't be empty, because at least your own wallet will
        -- be in there. But that might change when we have real wallet integration, and it's easy
        -- to forget cases like these, so it seems sensible to code for it in case.
        p [ classNames [ "p-4" ] ] [ text "You do not have any contacts." ]
      else
        ul_ $ contactLi <$> toUnfoldable walletLibrary
    , button
        [ classNames $ Css.primaryButton <> Css.withIcon Icon.NewContact <> Css.fixedBottomRight
        , onClick_ $ SetCardSection $ NewWallet Nothing
        ]
        [ text "New contact" ]
    ]
  where
  contactLi (nickname /\ walletDetails) =
    li
      [ classNames [ "px-4", "py-2", "border-gray", "border-b", "hover:cursor-pointer", "hover:text-purple" ]
      , onClick_ $ SetCardSection $ ViewWallet walletDetails
      ]
      [ text nickname ]

walletDetailsCard :: forall p. WalletDetails -> WalletDetails -> HTML p Action
walletDetailsCard currentWallet walletDetails =
  let
    walletNickname = walletDetails ^. _walletNickname

    companionAppId = walletDetails ^. _companionAppId

    companionAppIdString = UUID.toString $ unwrap companionAppId

    isCurrentWallet = walletNickname == currentWallet ^. _walletNickname
  in
    div
      [ classNames [ "grid", "grid-rows-1fr-auto", "p-4", "gap-4" ] ]
      [ div_
          [ h3
              [ classNames [ "text-lg", "font-semibold", "mb-4" ] ]
              [ text walletNickname ]
          , h4
              [ classNames [ "text-sm", "font-semibold" ] ]
              [ text "Demo wallet key" ]
          , div
              [ classNames [ "flex", "items-center", "justify-between" ] ]
              [ span
                  [ classNames [ "font-mono", "text-xs", "whitespace-nowrap" ] ]
                  [ text companionAppIdString ]
              , span
                  -- I don't understand why I have to set the width of this - for some reason it
                  -- grows very wide if I don't :/
                  [ classNames [ "cursor-pointer", "w-6" ]
                  , onClick_ $ ClipboardAction $ Clipboard.CopyToClipboard companionAppIdString
                  ]
                  [ icon_ Icon.Copy ]
              ]
          , walletIdTip
          ]
      , div
          [ classNames [ "flex", "gap-4" ] ]
          [ a
              [ classNames $ Css.button <> [ "text-center" ]
              , onClick_ $ SetCardSection Home
              ]
              [ text "Back" ]
          , if isCurrentWallet then
              span
                [ classNames $ Css.button <> [ "flex-1", "text-center", "border-2", "border-green", "text-green" ] ]
                [ text "Using this wallet" ]
            else
              button
                [ classNames $ Css.primaryButton <> [ "flex-1", "text-center" ]
                , onClick_ $ UseWallet walletNickname companionAppId
                ]
                [ text "Use this wallet" ]
          ]
      ]

newWalletCard ::
  forall m.
  MonadAff m =>
  WalletLibrary ->
  Maybe String ->
  State ->
  ComponentHTML Action ChildSlots m
newWalletCard walletLibrary mTokenName state@{ remoteWalletInfo, walletNickname, walletId } =
  let
    walletNicknameInputId = "newWalletNickname"

    walletNicknameProps =
      let
        props = TInput.defaultProps walletNickname (parseWalletNickname walletLibrary)
      in
        props
          { id_ = Just walletNicknameInputId
          , placeholder = Just "Nickname"
          }

    walletIdInputId = "newWalletId"

    walletIdProps =
      let
        props = TInput.defaultProps walletId (parseWalletId remoteWalletInfo walletLibrary)
      in
        props
          { id_ = Just walletIdInputId
          , placeholder = Just "Wallet ID"
          }

    renderInput ::
      String ->
      String ->
      ComponentHTML Action ChildSlots m ->
      ComponentHTML Action ChildSlots m
    renderInput label_ id_ input =
      div
        [ classNames $ Css.hasNestedLabel <> [ "mb-4" ] ]
        [ label [ classNames Css.nestedLabel, for id_ ] [ text label_ ]
        , input
        ]
  in
    div
      [ classNames [ "grid", "grid-rows-1fr-auto", "p-4", "gap-4" ] ]
      [ div_
          [ renderInput
              "Wallet nickname"
              walletNicknameInputId
              $ TInput.render dashboardWalletDataNicknameSlot unit walletNicknameProps
              $ TInput.defaultHandleMessage WalletNicknameChanged
          , renderInput
              "Demo wallet ID"
              walletIdInputId
              $ TInput.render dashboardWalletDataIdSlot unit walletIdProps
              $ TInput.defaultHandleMessage WalletIdChanged
          ]
      , div
          [ classNames [ "flex", "gap-4" ] ]
          [ a
              [ classNames $ Css.button <> [ "flex-1", "text-center" ]
              , onClick_ case mTokenName of
                  Just _ -> CancelNewContactForRole
                  Nothing -> SetCardSection Home
              ]
              [ text "Back" ]
          , button
              ( [ classNames $ Css.primaryButton <> [ "flex-1" ] ]
                  <> case parseWalletDetails state of
                      Just walletDetails ->
                        [ disabled false
                        , onClick_ $ SaveWallet mTokenName walletDetails
                        ]
                      _ -> [ disabled true ]
              )
              [ text "Save" ]
          ]
      ]

walletIdTip :: forall p a. HTML p a
walletIdTip =
  p
    [ classNames [ "text-xs", "font-semibold" ] ]
    [ text "Tip: Copy and share your demo wallet ID with others so they can add you to their contracts" ]
