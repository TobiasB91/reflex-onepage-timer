{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Data.FileEmbed
import           Reflex.Dom
import           Timer

main :: IO ()
main = mainWidgetWithCss css $ timerWidget (15*60) where
  css = $(embedFile "css/style.css")
