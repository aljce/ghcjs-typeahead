{-# LANGUAGE RankNTypes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Typeahead.Config where

import Prelude hiding (splitAt)
import Reflex.Dom hiding (Element, display)
import Control.Lens hiding (index,Index)
import Control.Applicative
import Data.Sequences
import Data.Default
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Internal.Search as T
import qualified Data.List as L

data Matches o = Matches {
  _original :: o,
  _parity   :: Bool,
  _matches  :: [o] }

instance (Eq o) => Eq (Matches o) where
  (Matches a _ ams) == (Matches b _ bms) = length ams == length bms && a == b

instance (Ord o) => Ord (Matches o) where
  compare (Matches a _ ams) (Matches b _ bms) = compare (length ams) (length bms) <> compare a b

data TypeaheadConfig t m f o = TypeaheadConfig {
    _elements        :: Dynamic t (f o)
  , _maxDisplay      :: Dynamic t Int
  , _minQueryLength  :: Dynamic t Int
  , _showHintOnFocus :: Bool
  , _scrollHeight    :: Dynamic t Int
  , _matcher         :: T.Text -> o -> Matches o
  , _sorter          :: f (Matches o) -> f (Matches o)
  , _highlighter     :: Matches o -> m ()
  , _appendTo        :: Dynamic t (Maybe (m ()))
  , _fitToElement    :: Bool
  , _defaultElem     :: Maybe o }

concat <$> mapM makeLenses [
  ''Matches,
  ''TypeaheadConfig ]

buildConfig :: (DomBuilder t m, Alternative f, IsSequence o, Index o ~ Int) => (o -> T.Text) -> TypeaheadConfig t m f o
buildConfig display = TypeaheadConfig {
    _elements        = pure empty
  , _maxDisplay      = pure 8
  , _minQueryLength  = pure 1
  , _showHintOnFocus = False
  , _scrollHeight    = pure 0
  , _matcher         = defMatcher
  , _sorter          = id
  , _highlighter     = defHighlighter
  , _appendTo        = pure Nothing
  , _fitToElement    = False
  , _defaultElem     = Nothing }
  where defMatcher needle = toMatches <*> T.indices needle . display
          where toMatches haystack is = Matches haystack (startStrong is) (splitLoop haystack is)
                startStrong [0] = True
                startStrong  _  = False
                splitLoop haystack [] = [haystack]
                splitLoop haystack (i : is) = case splitAt i haystack of
                  (match,rest) -> match : splitLoop rest is
        defHighlighter (Matches _ parityM matchesM) = loop parityM matchesM
          where loop _ [] = return ()
                loop True (m:ms) = do
                  (el "strong" . text . display) m
                  loop False ms
                loop False (m:ms) = do
                  (text . display) m
                  loop True ms

instance (DomBuilder t m, f ~ [], o ~ T.Text) => Default (TypeaheadConfig t m f o) where
  def = buildConfig id & sorter .~ L.sort
