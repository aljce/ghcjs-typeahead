{-# LANGUAGE OverloadedStrings #-}
module Main where

import Reflex.Dom

import Typeahead.Input
import Typeahead.Config

main :: IO ()
main = mainWidgetInElementById "reflex" $ do
  el "h1" $ text "Reflex"
  typeaheadInput (def & elements .~ pure ["kyle","andrew","alexa","kate"])
