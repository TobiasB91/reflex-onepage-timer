{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Data.FileEmbed
import           Reflex.Dom
import           Timer

main :: IO ()
main = mainWidgetWithCss css $ timerWidget 30 where
  css = $(embedFile "css/style.css")
