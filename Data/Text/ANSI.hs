{-# LANGUAGE CPP #-}
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
  , brightBlack
  , brightRed
  , brightGreen
  , brightYellow
  , brightBlue
  , brightMagenta
  , brightCyan
  , brightWhite
  , rgb

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
  , rgbBg

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

#if ! MIN_VERSION_base (4,13,0)
import Data.Semigroup ((<>))
#endif
import Data.Text
import Data.Word (Word8)
import Foreign.C (CInt(CInt))
import System.IO.Unsafe (unsafePerformIO)
import Text.Builder (Builder)

import qualified Text.Builder as Builder

-- $intro
--
-- Text styling for ANSI terminals using SGR codes, as defined by the
-- <https://www.ecma-international.org/publications/files/ECMA-ST/Ecma-048.pdf ECMA-48>
-- standard.
--
-- Supports foreground\/background color, bold\/faint intensity, italic,
-- single\/double underline, strikethrough, frame, encircle, and overline escape
-- sequences. Some styles may not work on your terminal.
--
-- Also features terminal detection, so redirecting styled output to a file will
-- automatically strip the ANSI escape sequences.

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

-- | Black foreground.
black           = surround "30"  "39"
-- | Red foreground.
red             = surround "31"  "39"
-- | Green foreground.
green           = surround "32"  "39"
-- | Yellow foreground.
yellow          = surround "33"  "39"
-- | Blue foreground.
blue            = surround "34"  "39"
-- | Magenta foreground.
magenta         = surround "35"  "39"
-- | Cyan foreground.
cyan            = surround "36"  "39"
-- | White foreground.
white           = surround "37"  "39"
-- | Bright black foreground.
brightBlack     = surround "90"  "39"
-- | Bright red foreground.
brightRed       = surround "91"  "39"
-- | Bright green foreground.
brightGreen     = surround "92"  "39"
-- | Bright yellow foreground.
brightYellow    = surround "93"  "39"
-- | Bright blue foreground.
brightBlue      = surround "94"  "39"
-- | Bright magenta foreground.
brightMagenta   = surround "95"  "39"
-- | Bright cyan foreground.
brightCyan      = surround "96"  "39"
-- | Bright white foreground.
brightWhite     = surround "97"  "39"
-- | Black background.
blackBg         = surround "40"  "49"
-- | Red background.
redBg           = surround "41"  "49"
-- | Green background.
greenBg         = surround "42"  "49"
-- | Yellow background.
yellowBg        = surround "43"  "49"
-- | Blue background.
blueBg          = surround "44"  "49"
-- | Magenta background.
magentaBg       = surround "45"  "49"
-- | Cyan background.
cyanBg          = surround "46"  "49"
-- | White background.
whiteBg         = surround "47"  "49"
-- | Bright black background.
brightBlackBg   = surround "100" "49"
-- | Bright red background.
brightRedBg     = surround "101" "49"
-- | Bright green background.
brightGreenBg   = surround "102" "49"
-- | Bright yellow background.
brightYellowBg  = surround "103" "49"
-- | Bright blue background.
brightBlueBg    = surround "104" "49"
-- | Bright magenta background.
brightMagentaBg = surround "105" "49"
-- | Bright cyan background.
brightCyanBg    = surround "106" "49"
-- | Bright white background.
brightWhiteBg   = surround "107" "49"

-- | RGB foreground.
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

-- | RGB background.
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

-- | __Bold__ style (high intensity).
bold            = surround "1"  "22"
-- | Faint style (low intensity).
faint           = surround "2"  "22"
-- | /Italic/ style.
italic          = surround "3"  "32"
-- | U̲n̲d̲e̲r̲l̲i̲n̲e̲ style.
underline       = surround "4"  "24"
-- | D̳o̳u̳b̳l̳e̳ ̳u̳n̳d̳e̳r̳l̳i̳n̳e̳ style.
doubleUnderline = surround "21" "24"
-- | S̶t̶r̶i̶k̶e̶t̶h̶r̶o̶u̶g̶h̶ style.
strikethrough   = surround "9"  "29"
-- | Frame style.
frame           = surround "51" "54"
-- | Encircle style.
encircle        = surround "52" "54"
-- | O̅v̅e̅r̅l̅i̅n̅e̅ style.
overline        = surround "53" "55"



--------------------------------------------------------------------------------

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

{-# NOINLINE isatty #-}
isatty :: Bool
isatty =
  unsafePerformIO (c_isatty 1) == 1

foreign import ccall unsafe "isatty"
  c_isatty :: CInt -> IO CInt

-- Collapse surround/surround to a single surround before phase 1
{-# RULES
  "surround/surround" [~1]
  forall a b c d s.
  surround a b (surround c d s) =
  surround (a <> semi <> c) (b <> semi <> d) s
#-}
