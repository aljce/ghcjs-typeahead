{-# LANGUAGE TypeFamilies, FlexibleContexts, OverloadedStrings, LambdaCase, ScopedTypeVariables, RecursiveDo #-}
module Typeahead.Input where

import Reflex.Dom
import Control.Lens hiding (elements,Index)
import Control.Monad.Fix
import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import Data.Bool (bool)

import Typeahead.Config

data Direction k = Up | Down | Pos k

toDirection :: (Reflex t) => Event t Int -> Event t (Direction k)
toDirection = fmapMaybe (\case
  40 -> Just Up
  38 -> Just Down
  _  -> Nothing)

moveToDirection :: (MonadFix m, MonadHold t m, Reflex t, Ord k) =>
                   k -> Dynamic t (M.Map k v) -> Event t (Direction k) -> m (Dynamic t k)
moveToDirection defKey dynKeys dirs = do
  startKey <- genStartKey <$> sample (current dynKeys)
  foldDynM update startKey dirs
  where genStartKey = maybe defKey (fst . fst) . M.minViewWithKey
        update (Pos k) _   = return k
        update Up curKey   = fmap (\keys ->
          maybe (genStartKey keys) fst (M.lookupGT curKey keys)) (sample (current dynKeys))
        update Down curKey = fmap (\keys ->
          maybe (maybe defKey (fst . fst) (M.maxViewWithKey keys)) fst (M.lookupLT curKey keys)) (sample (current dynKeys))

search :: forall t m f o. (Reflex t) => TypeaheadConfig t m f o -> Dynamic t T.Text -> Dynamic t (M.Map Int (Matches o))
search conf inputValue = calculate
  <$> conf ^. minQueryLength
  <*> conf ^. maxDisplay
  <*> inputValue
  <*> conf ^. elements
  where calculate :: Int -> Int -> T.Text -> f o -> M.Map Int (Matches o)
        calculate minQLen maxD needle =
          M.filter (\m -> minQLen <= m ^. numMatches) . (conf ^. sorter) needle

-- TODO: Rename
listEl :: (DomBuilderSpace m ~ GhcjsDomSpace, DomBuilder t m, PostBuild t m) =>
          (Matches o -> m ()) -> Dynamic t (Matches o) -> Dynamic t Bool -> m (Event t ())
listEl highlight dynMatches active = do
  (li,_) <- elDynAttr' "li" (bool M.empty ("class" =: "active") <$> active) $
    dyn (fmap displayMatch dynMatches)
  return (domEvent Click li)
  where displayMatch = elAttr "a" aAttrs . highlight
        aAttrs       = M.fromList [("class","dropdown-item"),("href","#"),("role","option")]

listAttrs :: (Reflex t) => Dynamic t (M.Map Int v) -> Dynamic t (M.Map T.Text T.Text)
listAttrs dynElems = genAttrs <$> dynElems
  where genAttrs elems = baseAttrs `M.union` ("style" =: bool "display: block" "display: none" (M.null elems))
        baseAttrs = M.fromList [("class","typeahead dropdown-menu"),("role","listbox")]
typeaheadInput :: (MonadFix m, DomBuilderSpace m ~ GhcjsDomSpace,
                   DomBuilder t m, PostBuild t m, MonadHold t m) => TypeaheadConfig t m f o -> m ()
typeaheadInput conf = do
  input <- textInput (conf ^. textInputConfig)
  let elems = search conf (input ^. textInput_value)
  rec active <- moveToDirection 0 elems (leftmost [fmap Pos clickedEls, toDirection (input ^. textInput_keydown)])
      clickedEls <- elDynAttr "ul" (listAttrs elems) $ selectViewListWithKey_ active elems
                      (\_ -> listEl (conf ^. highlighter))
  return ()
