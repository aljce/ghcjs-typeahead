{-# LANGUAGE RankNTypes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Typeahead.Config where

import Reflex.Dom hiding (display)
import Control.Lens
import Control.Applicative (Alternative(..))
import Data.Default
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Internal.Search as T
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

data TypeaheadConfig t m f o = TypeaheadConfig {
    _elements        :: Dynamic t (f o)
  , _maxDisplay      :: Dynamic t Int
  , _minQueryLength  :: Dynamic t Int
  , _textInputConfig :: TextInputConfig t
  , _showHintOnFocus :: Bool
  , _scrollHeight    :: Dynamic t Int
  , _sorter          :: T.Text -> f o -> M.Map Int (Matches o)
  , _highlighter     :: Matches o -> m ()
  , _appendTo        :: Dynamic t (Maybe (m ()))
  , _fitToElement    :: Bool
  , _defaultElem     :: Maybe o }

concat <$> mapM makeLenses [
  ''Matches,
  ''TypeaheadConfig ]

buildConfig :: (DomBuilder t m, Foldable f, Alternative f)
            => (o -> T.Text) -> TypeaheadConfig t m f o
buildConfig display = TypeaheadConfig {
    _elements        = pure empty
  , _maxDisplay      = pure 8
  , _minQueryLength  = pure 1
  , _textInputConfig = def
  , _showHintOnFocus = False
  , _scrollHeight    = pure 0
  , _sorter          = defSorter
  , _highlighter     = defHighlighter
  , _appendTo        = pure Nothing
  , _fitToElement    = False
  , _defaultElem     = Nothing }
  where defSorter needle = case T.null needle of
                             False -> toIntMap . toMatchesMap
                             True  -> const M.empty
          where toIntMap = snd . foldr (\v (n,m) -> (n+1,M.insert n v m)) (0,M.empty)
                toMatchesMap = foldr (S.insert . genMatches) S.empty
                genMatches haystack = Matches haystack (fmap fst (L.uncons ms) == Just needle)
                                              (length s `div` 2) ms
                  where s  = T.splitOn needle (display haystack)
                        ms = L.filter (not . T.null) (L.intersperse needle s)
        defHighlighter (Matches _ parityM _ matchesM) = loop parityM matchesM
          where loop _ [] = return ()
                loop True (m:ms) = do
                  (el "strong" . text) m
                  loop False ms
                loop False (m:ms) = do
                  text m
                  loop True ms

instance (DomBuilder t m, Foldable f, Alternative f, o ~ T.Text) => Default (TypeaheadConfig t m f o) where
  def = buildConfig id
