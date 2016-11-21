{-# LANGUAGE TypeFamilies, TemplateHaskell, OverloadedStrings, LambdaCase, ScopedTypeVariables, RecursiveDo #-}
module Typeahead.Input where

import Reflex.Dom
import Data.Functor.Misc (WrapArg(..))
import Control.Lens hiding (elements,Index)
import Control.Monad.Fix
import qualified Data.Map.Lazy as M
import Data.Dependent.Map (DSum(..))
import qualified Data.Dependent.Map as DM
import qualified Data.Text as T
import Data.Bool (bool)

import Typeahead.Config

data Index k = Up | Down | Reload | Pos k

arrowKeys :: (Reflex t) => Event t Int -> Event t (Index k)
arrowKeys = fmapMaybe (\case
  40 -> Just Up
  38 -> Just Down
  _  -> Nothing)

updateIndex :: (MonadFix m, MonadHold t m, Reflex t, Ord k) =>
                   k -> Behavior t (M.Map k v) -> [Event t (Index k)] -> m (Dynamic t k)
updateIndex defKey keysB indexEs = do
  startKey <- genStartKey <$> sample keysB
  foldDynM update startKey (leftmost indexEs)
  where genStartKey = maybe defKey (fst . fst) . M.minViewWithKey
        update (Pos k) _   = return k
        update Up curKey   = fmap (\keys ->
          maybe (genStartKey keys) fst (M.lookupGT curKey keys)) (sample keysB)
        update Down curKey = fmap (\keys ->
          maybe (maybe defKey (fst . fst) (M.maxViewWithKey keys)) fst (M.lookupLT curKey keys)) (sample keysB)
        update Reload _    = genStartKey <$> sample keysB

searched :: forall t m o. (Reflex t) => TypeaheadConfig t m o -> Dynamic t T.Text -> Dynamic t (M.Map Int (Matches o))
searched conf inputValue = calculate
  <$> conf ^. minQueryLength
  <*> conf ^. maxDisplay
  <*> (conf ^. searcher) inputValue
  where calculate :: Int -> Int -> M.Map Int (Matches o) -> M.Map Int (Matches o)
        calculate minQLen maxD =
          M.filter (\m -> minQLen <= m ^. numMatches)

-- TODO: Rename
liEl :: (DomBuilderSpace m ~ GhcjsDomSpace, DomBuilder t m, PostBuild t m) =>
        (Matches o -> m ()) -> Dynamic t (Matches o) -> Dynamic t Bool ->
        m (Event t (DM.DMap (WrapArg EventResult EventName) Identity))
liEl highlight dynMatches active = do
  (li,_) <- elDynAttr' "li" (bool M.empty ("class" =: "active") <$> active) $
    dyn (fmap displayMatch dynMatches)
  (return . merge) (DM.fromList [WrapArg Mouseover :=> EventResult <$> domEvent Mouseover li,
                                 WrapArg Click :=> EventResult <$> domEvent Click li])
  where displayMatch = elAttr "a" aAttrs . highlight
        aAttrs       = M.fromList [("class","dropdown-item"),("href","#"),("role","option")]

listAttrs :: (MonadHold t m, Reflex t) => [Event t Bool] -> m (Dynamic t (M.Map T.Text T.Text))
listAttrs = fmap (fmap genAttrs) . holdDyn True . leftmost
  where genAttrs showP = baseAttrs `M.union` ("style" =: bool "display: block" "display: none" showP)
        baseAttrs = M.fromList [("class","typeahead dropdown-menu"),("role","listbox")]

updateInputConfig :: (Reflex t) => (o -> T.Text) -> Event t o -> TextInputConfig t -> TextInputConfig t
updateInputConfig dis clicked = over textInputConfig_setValue updateSetEvent
  where updateSetEvent oldE = leftmost [fmap dis clicked, oldE]

data TypeaheadInput t o = TypeaheadInput {
    _selected      :: Event t o
 ,  _textInputData :: TextInput t }

makeLenses ''TypeaheadInput

typeaheadInput :: (MonadFix m, DomBuilderSpace m ~ GhcjsDomSpace,
                   DomBuilder t m, PostBuild t m, MonadHold t m) => TypeaheadConfig t m o -> m (TypeaheadInput t o)
typeaheadInput conf = do
  rec let clicked = attachPromptlyDynWithMaybe (\m i -> fmap (view original) (M.lookup i m)) elems
                                               (getEvent (WrapArg Click) listEvents)
          -- selected     = leftmost [clicked]
      input <- textInput (updateInputConfig (conf ^. shown) clicked (conf ^. textInputConfig))
      let elems = searched conf (input ^. textInput_value)
      active <- updateIndex 0 (current elems)
        [ Reload <$ updated elems
        , Pos <$> getEvent (WrapArg Mouseover) listEvents
        , arrowKeys (input ^. textInput_keydown)]
      dynListAttrs <- listAttrs
        [ M.null <$> updated elems ]
      listEvents <- elDynAttr "ul" dynListAttrs $ selectViewListWithKey active elems
        (const (liEl (conf ^. highlighter)))
  return (TypeaheadInput clicked input)
  where getEvent e = fmapMaybe (\(k, dm) -> if DM.member e dm then Just k else Nothing)
