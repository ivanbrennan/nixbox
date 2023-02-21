{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -Werror #-}

-- layers have numbers, starting at 1
-- the default layer is 1
-- each workspace's metadata is a set of layer numbers
-- if the set is empty, we'll default to a set containing just the default layer number
-- how do we determine a workspace's tag and position within a layer?
-- we can additionally use extensible state to attach optional names to layer numbers

-- TODO: attach layerid to workspace layout instead of extensible state

module XMonad.Experimental.Layout.WorkspaceLayers
  ( WorkspaceLayers,
    LayerId (..),
    LayerName (..),
    currentLayer,
    defaultLayerId,
    workspaceLayers,
    getWorkspaceLayerIds,
    addWorkspaceToLayer,
    marshall,
    unmarshall,
    unmarshallL,
    unmarshallW,
    marshallPP,
    removeWorkspaceFromLayer,
    assignSingleLayer,
    setCurrentScreenLayer,
    updateCurrentScreenLayer,
    updateScreenLayer,
    switchToLayer,
  )
where

import Control.Arrow ((***))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set, delete, insert, singleton)
import Text.Read (readMaybe)
import XMonad
  ( LayoutClass, Message, Window, WindowSpace, WorkspaceId, X, fromMessage,
    sendMessage, ScreenId, ExtensionClass, initialValue, extensionType,
    StateExtension (PersistentExtension), withWindowSet,
  )
import XMonad.Prelude (Alt, getAlt)
import XMonad.Prompt (XPrompt, XPConfig, mkXPrompt, showXPrompt)
import XMonad.Hooks.StatusBar.PP (PP, ppRename, ppSort)
import XMonad.Layout.LayoutModifier
  ( LayoutModifier, ModifiedLayout(ModifiedLayout), handleMess,
  )
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Experimental.Layout.Inspect
  ( InspectLayout, InspectResult, inspectLayout, inspectWorkspace,
  )

newtype LayerId = LayerId { unLayerId :: Int }
  deriving (Show, Read, Eq, Ord)

defaultLayerId :: LayerId
defaultLayerId = LayerId 1

marshall :: LayerId -> WorkspaceId -> WorkspaceId
marshall (LayerId li) lws = show li ++ '_':lws

unmarshall :: WorkspaceId -> (LayerId, WorkspaceId)
unmarshall = ((LayerId . read) *** drop 1) . break (=='_')

unmarshallL :: WorkspaceId -> LayerId
unmarshallL = fst . unmarshall

unmarshallW :: WorkspaceId -> WorkspaceId
unmarshallW = snd . unmarshall

marshallPP :: (WorkspaceId -> WorkspaceId) -> ScreenId -> PP -> X PP
marshallPP f s pp = do
  li <- currentLayer s
  pure $ pp { ppRename = ppRename pp . unmarshallW,
              ppSort = (workspacesOn f li .) <$> ppSort pp
            }

workspacesOn :: (WorkspaceId -> WorkspaceId) -> LayerId -> [WindowSpace] -> [WindowSpace]
workspacesOn f li = filter (\ws -> unmarshallL (f $ W.tag ws) == li)

newtype LayerState = LayerState { layersByScreen :: Map ScreenId LayerId }
  deriving (Read, Show)

instance ExtensionClass LayerState where
  initialValue = LayerState mempty
  extensionType = PersistentExtension

currentLayer :: ScreenId -> X LayerId
currentLayer s =
  fmap
    (fromMaybe defaultLayerId . Map.lookup s)
    (XS.gets layersByScreen)

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
assignSingleLayer xP =
  mkXPrompt pr xP (const (return [])) (safely (sendMessage . AssignLayers . singleton . LayerId))
  where pr = LayerPrompt "Layer: "

-- TODO: A more useful function would be to prompt for a workspace to pull into the current layer.
addWorkspaceToLayer :: XPConfig -> X ()
addWorkspaceToLayer xP =
  mkXPrompt pr xP (const (return [])) (safely (sendMessage . AddToLayer . LayerId))
  where pr = LayerPrompt "Add to layer: "

removeWorkspaceFromLayer :: XPConfig -> X ()
removeWorkspaceFromLayer xP =
  mkXPrompt pr xP (const (return [])) (safely (sendMessage . RemoveFromLayer . LayerId))
  where pr = LayerPrompt "Remove from layer: "

setCurrentScreenLayer :: XPConfig -> X ()
setCurrentScreenLayer xP =
  -- TODO: completion list
  mkXPrompt pr xP (const (return [])) (safely (f . LayerId))
  where
    pr = LayerPrompt "Layer: "
    f li = withWindowSet $ \ws ->
      updateScreenLayer (W.screen . W.current $ ws) li

switchToLayer :: LayerId -> X ()
switchToLayer =
  updateCurrentScreenLayer

updateCurrentScreenLayer :: LayerId -> X ()
updateCurrentScreenLayer li =
  withWindowSet $ \ws ->
    updateScreenLayer (W.screen $ W.current ws) li

updateScreenLayer :: ScreenId -> LayerId -> X ()
updateScreenLayer si li =
  XS.modify (LayerState . Map.insert si li . layersByScreen)

safely :: Read a => (a -> X ()) -> String -> X ()
safely f str =
  maybe (pure ()) f (readMaybe str)
