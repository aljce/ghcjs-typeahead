{-# LANGUAGE TypeFamilies, FlexibleContexts, OverloadedStrings #-}
module Typeahead.Input where

import Prelude hiding (take, filter)
import Reflex.Dom
import Control.Lens hiding (elements,Index)
import Data.Map (fromList)
import Data.Sequences hiding (fromList)
import Data.Foldable
import Control.Monad.Fix
import Control.Monad.IO.Class

import Typeahead.Config

typeaheadInput :: (MonadFix m, MonadIO (Performable m), MonadHold t m, DomBuilder t m,
                   PostBuild t m, PerformEvent t m, DomBuilderSpace m ~ GhcjsDomSpace, Foldable f)
               => TypeaheadConfig t m f o -> m ()
typeaheadInput conf = do
  input <- textInput (conf ^. textInputConfig)
  let heightRow = 30
  virtualListWithSelection (fmap (*heightRow) (conf ^. maxDisplay))
                           heightRow
                           (fmap length (conf ^. elements))
                           0
                           never
                           "ul"
                           (pure listAttrs)
                           "li"
                           (pure mempty)
                           (\_ item bool -> do
                               dyn $ fmap (maybe (return ()) (conf ^. highlighter)) item
                               return ())
                           (elems input)
                           id
  return ()
  where listAttrs = fromList [("class","typeahead dropdown-menu"),("role","listbox")]
        elems input = (\needle -> fromList . zip [0..] . fmap ((conf ^. matcher) needle) . toList)
          <$> (input ^. textInput_value) <*> (conf ^. elements)
  
