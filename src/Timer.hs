{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecursiveDo              #-}

module Timer where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Fix
import           Data.Function
import           Data.Monoid
import           Data.Text
import           Data.Time.Clock
import           Reflex.Dom
import           Reflex.Dom.Time

foreign import javascript unsafe "screenfull.toggle()" toggleFullscreen :: IO ()

timerWidget :: Integer -> Widget a ()
timerWidget maxTime = do

  -- include extern javascript
  elAttr "script" ("src" =: "https://cdnjs.cloudflare.com/ajax/libs/screenfull.js/3.3.2/screenfull.min.js") blank

  eTick <- tickLossy 1.0 =<< liftJS getCurrentTime

  let
    mkClasses t
      | t // maxTime <= 0.5 = "container container-green"
      | t // maxTime <= 0.7 = "container container-yellow"
      | t // maxTime <= 0.95 = "container container-orange"
      | otherwise = "container container-red" where
          (//) = (/) `on` fromInteger

  rec
    dClasses <- elDynClass "div" dClasses $ do

      -- controls
      (eStart, ePause, eExpand) <- elClass "div" "controls" $ do
        eExpand <- aButton "Fullscreen" "fa fa-expand"
        performEvent_ $ liftJS toggleFullscreen <$ eExpand
        eStart <- aButton "Start" "fa fa-play"
        ePause <- aButton "Pause" "fa fa-pause"
        return (eStart, ePause, eExpand)

      -- footer
      elClass "div" "footer" $ do
        elClass "i" "fa fa-github" blank
        elAttr "a" ("href" =: "https://github.com/elbingobonito/reflex-onepage-timer") $
          text "Written in Haskell."

      -- main div with timer text
      elClass "div" "main" $ do
        let dTimer = timer eStart ePause eTick
        dClasses <- fmap mkClasses <$> dTimer
        elClass "h1" "timerText" $
          dynText =<< fmap (formatS maxTime . min maxTime) <$> dTimer
        return dClasses
  return ()

aButton :: DomBuilder t m => Text -> Text -> m (Event t ())
aButton label iconLabel = do
  (e,_) <- elClass' "span" "aButton" $ do
    elClass "i" iconLabel blank
    el "a" $ text label
  return $ domEvent Click e

timer :: (PerformEvent t m, MonadWidget t m, MonadFix m, MonadHold t m, Reflex t) =>
  Event t () -> Event t () -> Event t TickInfo -> m (Dynamic t Integer)
timer eStart ePause eTick = do
  beStartStop <- hold never . leftmost $ [ ((0+) <$ eTick) <$ ePause, ((1+) <$ eTick) <$ eStart ]
  let eSwitch = switch beStartStop
  foldDyn ($) 0 eSwitch

formatS :: Integer -> Integer -> Text
formatS max cur =
  case properFraction (fromInteger (max-cur) / 60) of
    (mins, s) ->  let secs = round (s * 60) in
      pack $ fill mins <> ":" <> fill secs where
        fill n = if n `elem` [0..9] then "0" <> show n else show n
