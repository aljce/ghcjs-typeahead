{-# LANGUAGE OverloadedStrings #-}
module Main where

import Reflex.Dom

import Typeahead.Input

main :: IO ()
main = mainWidgetInElementById "reflex" $ do
  el "h1" $ text "Reflex"
  typeaheadInput undefined
