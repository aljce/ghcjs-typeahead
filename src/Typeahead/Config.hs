{-# LANGUAGE RankNTypes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Typeahead.Config where

import Reflex.Dom hiding (display)
import Control.Lens hiding (elements)
import Control.Applicative (liftA2)
import Data.Default
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import qualified Data.List as L

data Matches o = Matches {
    _original   :: o
  , _parity     :: Bool
  , _numMatches :: Int
  , _matches    :: [T.Text] }

instance Eq (Matches o) where
  (Matches _ ap _ ams) == (Matches _ bp _ bms) = ap == bp && length ams == length bms

instance Ord (Matches o) where
  compare (Matches _ ap _ ams) (Matches _ bp _ bms) =
    compare ap bp <> compare (fmap T.toCaseFold ams) (fmap T.toCaseFold bms)

data TypeaheadConfig t m o = TypeaheadConfig {
    _maxDisplay      :: Dynamic t Int
  , _minQueryLength  :: Dynamic t Int
  , _textInputConfig :: TextInputConfig t
  , _showHintOnFocus :: Bool
  , _scrollHeight    :: Dynamic t Int
  , _searcher        :: Dynamic t T.Text -> Dynamic t (M.Map Int (Matches o)) 
  , _shown           :: o -> T.Text
  , _highlighter     :: Matches o -> m ()
  , _appendTo        :: Dynamic t (Maybe (m ()))
  , _fitToElement    :: Bool
  , _defaultElem     :: Maybe o }

concat <$> mapM makeLenses [
  ''Matches,
  ''TypeaheadConfig ]

buildConfig :: (DomBuilder t m) => (o -> T.Text) -> TypeaheadConfig t m o
buildConfig display = TypeaheadConfig {
    _maxDisplay      = pure 8
  , _minQueryLength  = pure 1
  , _textInputConfig = def
  , _showHintOnFocus = False
  , _scrollHeight    = pure 0
  , _searcher        = const (pure M.empty)
  , _shown           = display
  , _highlighter     = defHighlighter
  , _appendTo        = pure Nothing
  , _fitToElement    = False
  , _defaultElem     = Nothing }
  where defHighlighter (Matches _ parityM _ matchesM) = loop parityM matchesM
          where loop _ [] = return ()
                loop True (m:ms) = do
                  (el "strong" . text) m
                  loop False ms
                loop False (m:ms) = do
                  text m
                  loop True ms

buildSearcher :: (Foldable f, Reflex t) => (o -> T.Text) -> Dynamic t (f o) ->
                  Dynamic t T.Text -> Dynamic t (M.Map Int (Matches o))
buildSearcher display elements = liftA2 checkEmptyNeedle elements
  where checkEmptyNeedle elems needle = case T.null needle of
          False -> (toIntMap . toMatchesMap needle) elems
          True  -> M.empty
        toIntMap = snd . foldr (\v (n,m) -> (n+1,M.insert n v m)) (0,M.empty)
        toMatchesMap needle = foldr (S.insert . genMatches needle) S.empty
        genMatches needle haystack = Matches haystack (fmap fst (L.uncons ms) == Just needle)
                                      (length s `div` 2) ms
          where s  = T.splitOn (T.toCaseFold needle) ((T.toCaseFold . display) haystack)
                ms = L.filter (not . T.null) (L.intersperse needle s)

instance (DomBuilder t m, o ~ T.Text) => Default (TypeaheadConfig t m o) where
  def = buildConfig id
