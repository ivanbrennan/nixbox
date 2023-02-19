{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  XMonad.Layout.Inspect
-- Copyright   :  (c) 2020 Tomáš Janoušek <tomi@nomi.cz>
-- License     :  BSD3
--
-- Maintainer  :  Tomáš Janoušek <tomi@nomi.cz>
-- Stability   :  experimental
-- Portability :  unknown
--
-- TODO
--
module XMonad.Experimental.Layout.Inspect (
    -- * Usage
    -- $usage
    inspectCurrent,
    inspectTag,
    inspectWorkspace,

    -- * The 'InspectLayout' class
    InspectResult,
    InspectLayout(..),
    ) where

import Data.List (find)
import Type.Reflection

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier (ModifiedLayout(ModifiedLayout))

-- $usage
--
-- TODO: help for users

-- | TODO
inspectCurrent :: (LayoutClass l Window, InspectLayout i l Window)
               => l Window -> i -> X (InspectResult i)
inspectCurrent l i = gets (inspectWorkspace l i . w)
    where w = W.workspace . W.current . windowset

-- | TODO
inspectTag :: (LayoutClass l Window, InspectLayout i l Window)
           => l Window -> i -> WorkspaceId
           -> X (Maybe (InspectResult i))
inspectTag l i t = gets (fmap (inspectWorkspace l i) . mw)
    where mw = find ((t==) . W.tag) . W.workspaces . windowset

-- | TODO
inspectWorkspace :: (LayoutClass l Window, InspectLayout i l Window)
                 => l Window -> i -> WindowSpace -> InspectResult i
inspectWorkspace l i = inspectLayout i . asLayout l . W.layout

asLayout :: (LayoutClass l a, Typeable a) => l a -> Layout a -> l a
asLayout l (Layout l') = cast' l' `asTypeOf` l

cast' :: forall a b. (Typeable a, Typeable b) => a -> b
cast' x | Just HRefl <- ta `eqTypeRep` tb = x
        | otherwise = error $ "X.L.Inspect.cast': " ++ show ta ++ " /= " ++ show tb
  where
    ta = typeRep :: TypeRep a
    tb = typeRep :: TypeRep b

type family InspectResult i

-- | TODO: for layout/modifier authors
class Monoid (InspectResult i) => InspectLayout i l a where
    inspectLayout :: i -> l a -> InspectResult i

instance {-# OVERLAPPABLE #-} Monoid (InspectResult i) => InspectLayout i l a where
    inspectLayout _ _ = mempty

instance (InspectLayout i l1 a, InspectLayout i l2 a) => InspectLayout i (Choose l1 l2) a where
    inspectLayout i (Choose CL l1 _) = inspectLayout i l1
    inspectLayout i (Choose CR _ l2) = inspectLayout i l2

instance (InspectLayout i m a, InspectLayout i l a)
  => InspectLayout i (ModifiedLayout m l) a where
    inspectLayout i (ModifiedLayout m l) =
        inspectLayout i m `mappend` inspectLayout i l
