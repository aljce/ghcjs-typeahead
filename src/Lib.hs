{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Reflex
import Reflex.Dom

main :: IO ()
main = mainWidgetInElementById "reflex" (el "h1" $ text "Reflex")
