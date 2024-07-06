{-
  This module provides each screen with three layers of nine workspaces.

  Workspace tags take the form S_L:N, where S denotes the screen, L denotes a
  layer within that screen, and N denotes a position within that layer. For
  example, the first workspace in the first layer on screen 0 is tagged "0_1:1" 

  Operations are provided for navigating screens/layers/workspaces.
-}

module XMonad.Actions.ScreenLayers (
  LayerId(..),
  NominalId(..),
  layerIds,
  nominalIds,
  workspaceIds,
  countScreens,
  viewNominal,
  viewScreen,
  viewLayer,
  viewNextLayer,
  viewPrevLayer,
  viewRecentNominal,
  viewRecentLayer,
  shiftToNominal,
  shiftToLayer,
  shiftToScreen,
  layerWS,
  layerPP,
) where

{- base -}
import Data.Foldable (find)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

{- xmonad -}
import XMonad
  ( ScreenId (S), WindowSet, WindowSpace, WorkspaceId, X, screenWorkspace,
  )
import qualified XMonad.StackSet as W

{- xmonad-contrib -}
import XMonad.Actions.CycleWS (WSType, wsTagGroup)
import XMonad.Hooks.StatusBar.PP (PP, ppExtras, ppRename, ppSort)
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Util.NamedScratchpad (scratchpadWorkspaceTag)
import XMonad.Util.WorkspaceCompare (filterOutWs)


-- | A 'LayerId' identifies a group of workspaces within the scope of a given
-- screen.
newtype LayerId
  = LayerId { unLayerId :: String }
  deriving (Eq, Ord)

-- | A 'NominalId' represents the ID that users see, e.g. "1" to "9". It
-- identifies a workspace within the scope of a given layer on a given screen.
newtype NominalId
  = NominalId { unNominalId :: String }
  deriving (Eq, Ord)

layer1 :: LayerId
layer1 = LayerId "1"

layerIds :: [LayerId]
layerIds = layer1 : map (LayerId . show) [2 .. 3 :: Int]

nominalIds :: [NominalId]
nominalIds = map (NominalId . show) [1 .. 9 :: Int]

-- TODO: use control characters instead? e.g. '\0'
screenSeparator :: Char
screenSeparator = '_'

layerSeparator :: Char
layerSeparator = ':'

-- | Encode ScreenId, LayerId and NominalId into a WorkspaceId
toTag :: (ScreenId, (LayerId, NominalId)) -> WorkspaceId
toTag (S i, (LayerId l, NominalId n)) =
  show i
    <> (screenSeparator : l)
    <> (layerSeparator : n)

-- | Decode ScreenId, LayerId and NominalId from a WorkspaceId.
fromTag :: WorkspaceId -> (ScreenId, (LayerId, NominalId))
fromTag t =
  fromMaybe fallback parsed
  where
    fallback :: (ScreenId, (LayerId, NominalId))
    fallback = (S 0, (layer1, NominalId t))

    parsed :: Maybe (ScreenId, (LayerId, NominalId))
    parsed = do
      (s@(_ : _), (_ : t')) <- Just (break (== screenSeparator) t)
      (l@(_ : _), (_ : n))  <- Just (break (== layerSeparator) t')
      i                     <- readMaybe s
      Just (S i, (LayerId l, NominalId n))

-- | Given a number of screens, construct a list of workspace tags. The tags are
-- ordered such that when screen initialization occurs (in 'XMonad.launch') each
-- screen will be allocated a workspace it was intended to manage.
workspaceIds :: Int -> [WorkspaceId]
workspaceIds nScreens =
  [ toTag (s, (g, n))
    | n <- nominalIds,
      g <- layerIds,
      s <- map S [0 .. nScreens - 1]
  ]

screenId :: WorkspaceId -> ScreenId
screenId = fst . fromTag

layerId :: WorkspaceId -> LayerId
layerId = fst . snd . fromTag

nominalId :: WorkspaceId -> NominalId
nominalId = snd . snd . fromTag

eqScreenId :: ScreenId -> WindowSpace -> Bool
eqScreenId s = (s ==) . screenId . W.tag

eqLayerId :: LayerId -> WindowSpace -> Bool
eqLayerId l = (l ==) . layerId . W.tag

-- | View a workspace satisfying predicate @p@, giving priority to more recently
-- viewed non-empty workspaces.
view :: (WindowSpace -> Bool) -> WindowSet -> WindowSet
view p wset =
  maybe id (W.view . W.tag) (find p ws) wset
  where
    ws :: [WindowSpace]
    ws = recentWorkspaces wset

-- | Recency list of workspaces, with non-empty spaces given priority.
recentWorkspaces :: WindowSet -> [WindowSpace]
recentWorkspaces wset =
  filter (not . null . W.stack) ws ++ ws
  where
    ws :: [WindowSpace]
    ws = filterOutWs [scratchpadWorkspaceTag] (W.workspaces wset)

-- | View nominal workspace @n@ in the active layer on the current screen.
viewNominal :: NominalId -> WindowSet -> WindowSet
viewNominal n wset =
  let s = W.screen $ W.current wset
      l = layerId . W.tag . W.workspace $ W.current wset
      t = toTag (s, (l, n))
   in view ((== t) . W.tag) wset

-- | View the most recently viewed workspace in the active layer on the current
-- screen.
viewRecentNominal :: WindowSet -> WindowSet
viewRecentNominal wset =
  let s = W.screen $ W.current wset
      t = W.currentTag wset
      l = layerId t
      p x = eqLayerId l x && eqScreenId s x && t /= W.tag x
   in view p wset

-- | View layer @l@ on the current screen.
viewLayer :: LayerId -> WindowSet -> WindowSet
viewLayer l wset =
  let s = W.screen $ W.current wset
      p x = eqLayerId l x && eqScreenId s x
   in view p wset

-- | View the most recently viewed layer on the current screen.
viewRecentLayer :: WindowSet -> WindowSet
viewRecentLayer wset =
  let s = W.screen $ W.current wset
      l = layerId $ W.currentTag wset
      p x = not (eqLayerId l x) && eqScreenId s x
   in view p wset

-- | View screen @s@.
viewScreen :: ScreenId -> WindowSet -> WindowSet
viewScreen s wset =
  maybe id W.view (W.lookupWorkspace s wset) wset

-- | Cycle through workspaces within the active layer on the current screen.
layerWS :: WSType
layerWS = wsTagGroup layerSeparator

-- | Cycle to next layer on the current screen.
viewNextLayer :: WindowSet -> WindowSet
viewNextLayer = cycleLayer succ

-- | Cycle to previous layer on the current screen.
viewPrevLayer :: WindowSet -> WindowSet
viewPrevLayer = cycleLayer pred

cycleLayer :: (Int -> Int) -> WindowSet -> WindowSet
cycleLayer f wset =
  maybe id (viewLayer . nth . f) (elemIndex l layerIds) wset
  where
    l :: LayerId
    l = layerId . W.tag . W.workspace $ W.current wset

    nth :: Int -> LayerId
    nth n = head $ drop (n `mod` length layerIds) layerIds

-- | Shift the currently focused window to nominal workspace @n@ in the active
-- layer on the current screen.
shiftToNominal :: NominalId -> WindowSet -> WindowSet
shiftToNominal n wset =
  let s = W.screen $ W.current wset
      l = layerId . W.tag . W.workspace $ W.current wset
      t = toTag (s, (l, n))
   in W.shift t wset

-- | Shift the currently focused window to layer @l@ on the current screen.
shiftToLayer :: LayerId -> WindowSet -> WindowSet
shiftToLayer l wset =
  let s = W.screen $ W.current wset
      ws = recentWorkspaces wset
      p x = eqLayerId l x && eqScreenId s x
   in maybe id (W.shift . W.tag) (find p ws) wset

-- | Shift the currently focused window to screen @s@.
shiftToScreen :: ScreenId -> WindowSet -> WindowSet
shiftToScreen s wset =
  maybe id W.shift (W.lookupWorkspace s wset) wset

-- | Make a pretty printer aware of workspace layers.
layerPP :: (LayerId -> X (Maybe String)) -> ScreenId -> PP -> PP
layerPP indicator s pp = pp
  { ppRename = ppRename pp . unNominalId . nominalId,
    ppSort   = withLayer $ \l -> (. filter (onLayer l)) <$> ppSort pp,
    ppExtras = withLayer indicator : ppExtras pp
  }
  where
    withLayer :: (LayerId -> X a) -> X a
    withLayer f =
      (f . maybe layer1 layerId) =<< screenWorkspace s

    onLayer :: LayerId -> WindowSpace -> Bool
    onLayer l x = eqLayerId l x && eqScreenId s x
