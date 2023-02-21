{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -Werror #-}

-- layers have numbers, starting at 0
-- the default layer is 0
-- each workspace's metadata is a set of layer numbers
-- if the set is empty, we'll default to a set containing just the default layer number
-- how do we determine a workspace's tag and position within a layer?
-- we can additionally use extensible state to attach optional names to layer numbers

-- Layers
-- WorkspaceLayers

module XMonad.Experimental.Layout.WorkspaceLayers
  ( WorkspaceLayers,
    LayerId (..),
    LayerName (..),
    workspaceLayers,
    getWorkspaceLayerIds,
    addWorkspaceToLayer,
    removeWorkspaceFromLayer,
    assignSingleLayer,
  )
where

import Data.Set (Set, delete, insert, singleton)
import Text.Read (readMaybe)
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

newtype LayerId = LayerId { unLayerId :: Int }
  deriving (Show, Read, Eq, Ord)

newtype LayerName = LayerName { unLayerName :: String }
  deriving (Show, Read, Eq, Ord)

newtype AssignLayers = AssignLayers (Set LayerId)
instance Message AssignLayers

newtype AddToLayer = AddToLayer LayerId
instance Message AddToLayer

newtype RemoveFromLayer = RemoveFromLayer LayerId
instance Message RemoveFromLayer

data WorkspaceLayers a = WorkspaceLayers (Set LayerId)
  deriving (Show, Read)

instance LayoutModifier WorkspaceLayers a where
  handleMess (WorkspaceLayers layers) m
    | Just (AssignLayers lis)   <- fromMessage m = pure $ Just (WorkspaceLayers lis)
    | Just (AddToLayer li)      <- fromMessage m = pure $ Just (WorkspaceLayers (insert li layers))
    | Just (RemoveFromLayer li) <- fromMessage m = pure $ Just (WorkspaceLayers (delete li layers))
    | otherwise = pure Nothing

workspaceLayers :: LayoutClass l a => l a -> ModifiedLayout WorkspaceLayers l a
workspaceLayers = ModifiedLayout (WorkspaceLayers mempty)

data ReadLayers = ReadLayers
type instance InspectResult ReadLayers = Alt Maybe (Set LayerId)

instance InspectLayout ReadLayers WorkspaceLayers a where
  inspectLayout ReadLayers (WorkspaceLayers lis) = pure lis

getWorkspaceLayerIds :: (LayoutClass l Window, InspectLayout ReadLayers l Window)
                     => l Window -> WindowSpace -> Maybe (Set LayerId)
getWorkspaceLayerIds lay = getAlt . inspectWorkspace lay ReadLayers

newtype LayerPrompt = LayerPrompt String

instance XPrompt LayerPrompt where
  showXPrompt (LayerPrompt x) = x

-- | Prompt for a layer for the current workspace and set it.
assignSingleLayer :: XPConfig -> X ()
assignSingleLayer conf =
  mkXPrompt pr conf (const (return [])) (safely (sendMessage . AssignLayers . singleton . LayerId))
  where pr = LayerPrompt "Layer: "

-- TODO: A more useful function would be to prompt for a workspace to pull into the current layer.
addWorkspaceToLayer :: XPConfig -> X ()
addWorkspaceToLayer conf =
  mkXPrompt pr conf (const (return [])) (safely (sendMessage . AddToLayer . LayerId))
  where pr = LayerPrompt "Add to layer: "

removeWorkspaceFromLayer :: XPConfig -> X ()
removeWorkspaceFromLayer conf =
  mkXPrompt pr conf (const (return [])) (safely (sendMessage . RemoveFromLayer . LayerId))
  where pr = LayerPrompt "Remove from layer: "

safely :: Read a => (a -> X ()) -> String -> X ()
safely f str =
  maybe (pure ()) f (readMaybe str)
