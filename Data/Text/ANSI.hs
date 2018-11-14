{-# LANGUAGE OverloadedStrings #-}

module Data.Text.ANSI
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
  , rgb
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
  , rgbBg
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
  , faint
  , italic
  , underline
  , doubleUnderline
  , strikethrough
  , frame
  , encircle
  , overline
  ) where

import Data.Text
import Data.Word (Word8)
import Foreign.C (CInt(CInt))
import System.IO.Unsafe (unsafePerformIO)
import Text.Builder (Builder)

import qualified Text.Builder as Builder

-- $intro
--
-- Text styling for ANSI terminals. Supports foreground/background color,
-- bold/faint intensity, italic, underline, strikethrough, frame, encircle, and
-- overline escape sequences. Some styles may not work on your terminal.

{-# INLINABLE black           #-}
{-# INLINABLE red             #-}
{-# INLINABLE green           #-}
{-# INLINABLE yellow          #-}
{-# INLINABLE blue            #-}
{-# INLINABLE magenta         #-}
{-# INLINABLE cyan            #-}
{-# INLINABLE white           #-}
{-# INLINABLE brightBlack     #-}
{-# INLINABLE brightRed       #-}
{-# INLINABLE brightGreen     #-}
{-# INLINABLE brightYellow    #-}
{-# INLINABLE brightBlue      #-}
{-# INLINABLE brightMagenta   #-}
{-# INLINABLE brightCyan      #-}
{-# INLINABLE brightWhite     #-}
{-# INLINABLE blackBg         #-}
{-# INLINABLE redBg           #-}
{-# INLINABLE greenBg         #-}
{-# INLINABLE yellowBg        #-}
{-# INLINABLE blueBg          #-}
{-# INLINABLE magentaBg       #-}
{-# INLINABLE cyanBg          #-}
{-# INLINABLE whiteBg         #-}
{-# INLINABLE brightBlackBg   #-}
{-# INLINABLE brightRedBg     #-}
{-# INLINABLE brightGreenBg   #-}
{-# INLINABLE brightYellowBg  #-}
{-# INLINABLE brightBlueBg    #-}
{-# INLINABLE brightMagentaBg #-}
{-# INLINABLE brightCyanBg    #-}
{-# INLINABLE brightWhiteBg   #-}

black, red, green, yellow, blue, magenta, cyan, white, brightBlack, brightRed,
  brightGreen, brightYellow, brightBlue, brightMagenta, brightCyan,
  brightWhite, blackBg, redBg, greenBg,
  yellowBg, blueBg, magentaBg, cyanBg,
  whiteBg, brightBlackBg, brightRedBg,
  brightGreenBg, brightYellowBg, brightBlueBg,
  brightMagentaBg, brightCyanBg,
  brightWhiteBg :: Text -> Text

black           = surround "30"  "39"
red             = surround "31"  "39"
green           = surround "32"  "39"
yellow          = surround "33"  "39"
blue            = surround "34"  "39"
magenta         = surround "35"  "39"
cyan            = surround "36"  "39"
white           = surround "37"  "39"
brightBlack     = surround "90"  "39"
brightRed       = surround "91"  "39"
brightGreen     = surround "92"  "39"
brightYellow    = surround "93"  "39"
brightBlue      = surround "94"  "39"
brightMagenta   = surround "95"  "39"
brightCyan      = surround "96"  "39"
brightWhite     = surround "97"  "39"
blackBg         = surround "40"  "49"
redBg           = surround "41"  "49"
greenBg         = surround "42"  "49"
yellowBg        = surround "43"  "49"
blueBg          = surround "44"  "49"
magentaBg       = surround "45"  "49"
cyanBg          = surround "46"  "49"
whiteBg         = surround "47"  "49"
brightBlackBg   = surround "100" "49"
brightRedBg     = surround "101" "49"
brightGreenBg   = surround "102" "49"
brightYellowBg  = surround "103" "49"
brightBlueBg    = surround "104" "49"
brightMagentaBg = surround "105" "49"
brightCyanBg    = surround "106" "49"
brightWhiteBg   = surround "107" "49"

{-# INLINABLE rgb #-}
rgb :: Word8 -> Word8 -> Word8 -> Text -> Text
rgb r g b =
  surround
    ("38;2;" <>
     Builder.unsignedDecimal r <>
     semi <>
     Builder.unsignedDecimal g <>
     semi <>
     Builder.unsignedDecimal b)
    "39"

{-# INLINABLE rgbBg #-}
rgbBg :: Word8 -> Word8 -> Word8 -> Text -> Text
rgbBg r g b =
  surround
    ("48;2;" <>
     Builder.unsignedDecimal r <>
     semi <>
     Builder.unsignedDecimal g <>
     semi <>
     Builder.unsignedDecimal b)
    "49"

{-# INLINABLE bold            #-}
{-# INLINABLE faint           #-}
{-# INLINABLE italic          #-}
{-# INLINABLE underline       #-}
{-# INLINABLE doubleUnderline #-}
{-# INLINABLE strikethrough   #-}
{-# INLINABLE frame           #-}
{-# INLINABLE encircle        #-}
{-# INLINABLE overline        #-}

bold, faint, italic, underline, doubleUnderline, strikethrough, frame,
  encircle, overline :: Text -> Text

bold            = surround "1"  "22"
faint           = surround "2"  "22"
italic          = surround "3"  "32"
underline       = surround "4"  "24"
doubleUnderline = surround "21" "24"
strikethrough   = surround "9"  "29"
frame           = surround "51" "54"
encircle        = surround "52" "54"
overline        = surround "53" "55"


--------------------------------------------------------------------------------

{-# NOINLINE isatty #-}
isatty :: Bool
isatty =
  unsafePerformIO (c_isatty 1) == 1

foreign import ccall unsafe "isatty"
  c_isatty :: CInt -> IO CInt

-- Don't inline before phase 1
{-# NOINLINE [1] surround #-}
surround :: Builder -> Builder -> Text -> Text
surround open close text
  | isatty = Builder.run (esc <> open <> m <> Builder.text text <> esc <> close <> m)
  | otherwise = text

esc :: Builder
esc = "\ESC["

m, semi :: Builder
m    = Builder.char 'm'
semi = Builder.char ';'

-- Collapse surround/surround to a single surround before phase 1
{-# RULES
  "surround/surround" [~1]
  forall a b c d s.
  surround a b (surround c d s) =
  surround (a <> semi <> c) (b <> semi <> d) s
#-}
