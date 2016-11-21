{-# LANGUAGE OverloadedStrings #-}
module Main where

import Reflex.Dom

import Typeahead.Input
import Typeahead.Config

main :: IO ()
main = mainWidgetInElementById "reflex" $ do
  el "h1" $ text "Reflex"
  typeaheadInput (def & searcher .~ buildSearcher id (pure ["kyle","andrew","alexa","kate"]))
  el "p" $ text "This text ensure that the typeahead popup is modal like"
