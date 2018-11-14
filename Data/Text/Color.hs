{-# language OverloadedStrings #-}

module Data.Text.Color
  ( -- $intro

    -- * Foreground color
    black
  , red
  , green
  , yellow
  , blue
  , magenta
  , cyan
  , white
  , brightBlack
  , brightRed
  , brightGreen
  , brightYellow
  , brightBlue
  , brightMagenta
  , brightCyan
  , brightWhite

    -- * Background color
  , blackBg
  , redBg
  , greenBg
  , yellowBg
  , blueBg
  , magentaBg
  , cyanBg
  , whiteBg
  , brightBlackBg
  , brightRedBg
  , brightGreenBg
  , brightYellowBg
  , brightBlueBg
  , brightMagentaBg
  , brightCyanBg
  , brightWhiteBg

    -- * Style
  , bold
  , underlined
  ) where

import Data.Text
import Text.Builder (Builder)

import qualified Text.Builder as Builder

-- $intro
--
-- Please don't be put off by the lack of documentation, there's not much to
-- say ;)
--
-- Supports foreground color, background color, bold style, and underlined
-- style.

black, red, green, yellow, blue, magenta, cyan, white, brightBlack, brightRed,
  brightGreen, brightYellow, brightBlue, brightMagenta, brightCyan,
  brightWhite, blackBg, redBg, greenBg,
  yellowBg, blueBg, magentaBg, cyanBg,
  whiteBg, brightBlackBg, brightRedBg,
  brightGreenBg, brightYellowBg, brightBlueBg,
  brightMagentaBg, brightCyanBg,
  brightWhiteBg :: Text -> Text

black           = fg "30"
red             = fg "31"
green           = fg "32"
yellow          = fg "33"
blue            = fg "34"
magenta         = fg "35"
cyan            = fg "36"
white           = fg "37"
brightBlack     = fg "90"
brightRed       = fg "91"
brightGreen     = fg "92"
brightYellow    = fg "93"
brightBlue      = fg "94"
brightMagenta   = fg "95"
brightCyan      = fg "96"
brightWhite     = fg "97"
blackBg         = bg "40"
redBg           = bg "41"
greenBg         = bg "42"
yellowBg        = bg "43"
blueBg          = bg "44"
magentaBg       = bg "45"
cyanBg          = bg "46"
whiteBg         = bg "47"
brightBlackBg   = bg "100"
brightRedBg     = bg "101"
brightGreenBg   = bg "102"
brightYellowBg  = bg "103"
brightBlueBg    = bg "104"
brightMagentaBg = bg "105"
brightCyanBg    = bg "106"
brightWhiteBg   = bg "107"

bold, underlined :: Text -> Text
bold       = surround "1" "22"
underlined = surround "4" "24"


--------------------------------------------------------------------------------

fg, bg :: Builder -> Text -> Text
fg n = surround n "39"
bg n = surround n "49"

surround :: Builder -> Builder -> Text -> Text
surround open close text =
  Builder.run (esc <> open <> m <> Builder.text text <> esc <> close <> m)

esc :: Builder
esc =
  "\ESC["

m :: Builder
m =
  Builder.char 'm'
