{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module XMonad.Experimental.Layout.WorkScopes
  ( WorkScopes,
    ScopeName (..),
    workScopes,
    getWorkScopeNames,
    rescopeWorkspace,
  )
where

import Data.Set (Set, singleton)
import XMonad
  ( LayoutClass, Message, Window, WindowSpace, X, fromMessage, sendMessage,
  )
import XMonad.Prelude (Alt, getAlt)
import XMonad.Prompt (XPrompt, XPConfig, mkXPrompt, showXPrompt)
import XMonad.Layout.LayoutModifier
  ( LayoutModifier, ModifiedLayout(ModifiedLayout), handleMess,
  )
import XMonad.Experimental.Layout.Inspect
  ( InspectLayout, InspectResult, inspectLayout, inspectWorkspace,
  )

newtype ScopeName = ScopeName { unScopeName :: String }
  deriving (Show, Read, Eq, Ord)

newtype SetScopes = SetScopes (Set ScopeName)
instance Message SetScopes

data WorkScopes a = WorkScopes (Set ScopeName)
  deriving (Show, Read)

instance LayoutModifier WorkScopes a where
  handleMess (WorkScopes _) m
    | Just (SetScopes names) <- fromMessage m = pure $ Just (WorkScopes names)
    | otherwise = pure Nothing

workScopes :: LayoutClass l a => l a -> ModifiedLayout WorkScopes l a
workScopes = ModifiedLayout (WorkScopes mempty)

data ScopeLabel = ScopeLabel
type instance InspectResult ScopeLabel = Alt Maybe (Set ScopeName)

instance InspectLayout ScopeLabel WorkScopes a where
  inspectLayout ScopeLabel (WorkScopes names) = pure names

getWorkScopeNames :: (LayoutClass l Window, InspectLayout ScopeLabel l Window)
                  => l Window -> WindowSpace -> Maybe (Set ScopeName)
getWorkScopeNames lay = getAlt . inspectWorkspace lay ScopeLabel

newtype ScopePrompt = ScopePrompt String

instance XPrompt ScopePrompt where
  showXPrompt (ScopePrompt x) = x

-- | Prompt for a scope for the current workspace and set it.
rescopeWorkspace :: XPConfig -> X ()
rescopeWorkspace conf =
  mkXPrompt pr conf (const (return [])) (sendMessage . SetScopes . singleton . ScopeName)
  where pr = ScopePrompt "Scope name: "
