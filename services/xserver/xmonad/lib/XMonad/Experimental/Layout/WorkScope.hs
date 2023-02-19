{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module XMonad.Experimental.Layout.WorkScope
  ( WorkScope,
    workScopes,
    getWorkScopeStr,
    rescopeWorkspace,
  )
where

import XMonad
  ( LayoutClass, Message, Window, WindowSpace, X, fromMessage, sendMessage,
  )
import XMonad.Prelude (Alt (Alt), getAlt)
import XMonad.Prompt (XPrompt, XPConfig, mkXPrompt, showXPrompt)
import XMonad.Layout.LayoutModifier
  ( LayoutModifier, ModifiedLayout(ModifiedLayout), handleMess,
  )
import XMonad.Experimental.Layout.Inspect
  ( InspectLayout, InspectResult, inspectLayout, inspectWorkspace,
  )

newtype ChScope = ChScope String
instance Message ChScope

data WorkScope a = WorkScope String
  deriving (Show, Read)

instance LayoutModifier WorkScope a where
  handleMess (WorkScope _) m
    | Just (ChScope name) <- fromMessage m = pure $ Just (WorkScope name)
    | otherwise = pure Nothing

workScopes :: LayoutClass l a => l a -> ModifiedLayout WorkScope l a
workScopes = ModifiedLayout (WorkScope "")

data ScopeLabel = ScopeLabel
type instance InspectResult ScopeLabel = Alt Maybe String

instance InspectLayout ScopeLabel WorkScope a where
  inspectLayout ScopeLabel (WorkScope name) = pure name

getWorkScopeStr :: (LayoutClass l Window, InspectLayout ScopeLabel l Window)
                => l Window -> WindowSpace -> Maybe String
getWorkScopeStr lay = getAlt . inspectWorkspace lay ScopeLabel

newtype ScopePrompt = ScopePrompt String

instance XPrompt ScopePrompt where
  showXPrompt (ScopePrompt x) = x

-- | Prompt for a scope for the current workspace and set it.
rescopeWorkspace :: XPConfig -> X ()
rescopeWorkspace conf =
  mkXPrompt pr conf (const (return [])) (sendMessage . ChScope)
  where pr = ScopePrompt "Scope name: "
