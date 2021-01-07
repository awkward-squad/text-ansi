{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Builder.ANSI
  ( -- $intro

    -- * Foreground color
    black,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    brightBlack,
    brightRed,
    brightGreen,
    brightYellow,
    brightBlue,
    brightMagenta,
    brightCyan,
    brightWhite,
    rgb,

    -- * Background color
    blackBg,
    redBg,
    greenBg,
    yellowBg,
    blueBg,
    magentaBg,
    cyanBg,
    whiteBg,
    brightBlackBg,
    brightRedBg,
    brightGreenBg,
    brightYellowBg,
    brightBlueBg,
    brightMagentaBg,
    brightCyanBg,
    brightWhiteBg,
    rgbBg,

    -- * Style
    bold,
    faint,
    italic,
    underline,
    doubleUnderline,
    strikethrough,
    frame,
    encircle,
    overline,
  )
where

#if !MIN_VERSION_base(4,13,0)
import Data.Semigroup ((<>))
#endif
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder
import Data.Word (Word8)
import Foreign.C (CInt (CInt))
import System.IO.Unsafe (unsafePerformIO)

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

-- | Black foreground.
black :: Builder -> Builder
black =
  foreground "30"
{-# INLINE black #-}

-- | Red foreground.
red :: Builder -> Builder
red =
  foreground "31"
{-# INLINE red #-}

-- | Green foreground.
green :: Builder -> Builder
green =
  foreground "32"
{-# INLINE green #-}

-- | Yellow foreground.
yellow :: Builder -> Builder
yellow =
  foreground "33"
{-# INLINE yellow #-}

-- | Blue foreground.
blue :: Builder -> Builder
blue =
  foreground "34"
{-# INLINE blue #-}

-- | Magenta foreground.
magenta :: Builder -> Builder
magenta =
  foreground "35"
{-# INLINE magenta #-}

-- | Cyan foreground.
cyan :: Builder -> Builder
cyan =
  foreground "36"
{-# INLINE cyan #-}

-- | White foreground.
white :: Builder -> Builder
white =
  foreground "37"
{-# INLINE white #-}

-- | Bright black foreground.
brightBlack :: Builder -> Builder
brightBlack =
  foreground "90"
{-# INLINE brightBlack #-}

-- | Bright red foreground.
brightRed :: Builder -> Builder
brightRed =
  foreground "91"
{-# INLINE brightRed #-}

-- | Bright green foreground.
brightGreen :: Builder -> Builder
brightGreen =
  foreground "92"
{-# INLINE brightGreen #-}

-- | Bright yellow foreground.
brightYellow :: Builder -> Builder
brightYellow =
  foreground "93"
{-# INLINE brightYellow #-}

-- | Bright blue foreground.
brightBlue :: Builder -> Builder
brightBlue =
  foreground "94"
{-# INLINE brightBlue #-}

-- | Bright magenta foreground.
brightMagenta :: Builder -> Builder
brightMagenta =
  foreground "95"
{-# INLINE brightMagenta #-}

-- | Bright cyan foreground.
brightCyan :: Builder -> Builder
brightCyan =
  foreground "96"
{-# INLINE brightCyan #-}

-- | Bright white foreground.
brightWhite :: Builder -> Builder
brightWhite =
  foreground "97"
{-# INLINE brightWhite #-}

-- | RGB foreground.
rgb :: Word8 -> Word8 -> Word8 -> Builder -> Builder
rgb r g b =
  foreground ("38;2;" <> Builder.decimal r <> semi <> Builder.decimal g <> semi <> Builder.decimal b)
{-# INLINE rgb #-}

foreground :: Builder -> Builder -> Builder
foreground s =
  surround s "39"
{-# INLINE foreground #-}

-- | Black background.
blackBg :: Builder -> Builder
blackBg =
  background "40"
{-# INLINE blackBg #-}

-- | Red background.
redBg :: Builder -> Builder
redBg =
  background "41"
{-# INLINE redBg #-}

-- | Green background.
greenBg :: Builder -> Builder
greenBg =
  background "42"
{-# INLINE greenBg #-}

-- | Yellow background.
yellowBg :: Builder -> Builder
yellowBg =
  background "43"
{-# INLINE yellowBg #-}

-- | Blue background.
blueBg :: Builder -> Builder
blueBg =
  background "44"
{-# INLINE blueBg #-}

-- | Magenta background.
magentaBg :: Builder -> Builder
magentaBg =
  background "45"
{-# INLINE magentaBg #-}

-- | Cyan background.
cyanBg :: Builder -> Builder
cyanBg =
  background "46"
{-# INLINE cyanBg #-}

-- | White background.
whiteBg :: Builder -> Builder
whiteBg =
  background "47"
{-# INLINE whiteBg #-}

-- | Bright black background.
brightBlackBg :: Builder -> Builder
brightBlackBg =
  background "100"
{-# INLINE brightBlackBg #-}

-- | Bright red background.
brightRedBg :: Builder -> Builder
brightRedBg =
  background "101"
{-# INLINE brightRedBg #-}

-- | Bright green background.
brightGreenBg :: Builder -> Builder
brightGreenBg =
  background "102"
{-# INLINE brightGreenBg #-}

-- | Bright yellow background.
brightYellowBg :: Builder -> Builder
brightYellowBg =
  background "103"
{-# INLINE brightYellowBg #-}

-- | Bright blue background.
brightBlueBg :: Builder -> Builder
brightBlueBg =
  background "104"
{-# INLINE brightBlueBg #-}

-- | Bright magenta background.
brightMagentaBg :: Builder -> Builder
brightMagentaBg =
  background "105"
{-# INLINE brightMagentaBg #-}

-- | Bright cyan background.
brightCyanBg :: Builder -> Builder
brightCyanBg =
  background "106"
{-# INLINE brightCyanBg #-}

-- | Bright white background.
brightWhiteBg :: Builder -> Builder
brightWhiteBg =
  background "107"
{-# INLINE brightWhiteBg #-}

background :: Builder -> Builder -> Builder
background s =
  surround s "49"
{-# INLINE background #-}

-- | RGB background.
rgbBg :: Word8 -> Word8 -> Word8 -> Builder -> Builder
rgbBg r g b =
  background ("48;2;" <> Builder.decimal r <> semi <> Builder.decimal g <> semi <> Builder.decimal b)
{-# INLINE rgbBg #-}

-- | __Bold__ style (high intensity).
bold :: Builder -> Builder
bold =
  surround "1" "22"
{-# INLINE bold #-}

-- | Faint style (low intensity).
faint :: Builder -> Builder
faint =
  surround "2" "22"
{-# INLINE faint #-}

-- | /Italic/ style.
italic :: Builder -> Builder
italic =
  surround "3" "23"
{-# INLINE italic #-}

-- | U̲n̲d̲e̲r̲l̲i̲n̲e̲ style.
underline :: Builder -> Builder
underline =
  surround "4" "24"
{-# INLINE underline #-}

-- | D̳o̳u̳b̳l̳e̳ ̳u̳n̳d̳e̳r̳l̳i̳n̳e̳ style.
doubleUnderline :: Builder -> Builder
doubleUnderline =
  surround "21" "24"
{-# INLINE doubleUnderline #-}

-- | S̶t̶r̶i̶k̶e̶t̶h̶r̶o̶u̶g̶h̶ style.
strikethrough :: Builder -> Builder
strikethrough =
  surround "9" "29"
{-# INLINE strikethrough #-}

-- | Frame style.
frame :: Builder -> Builder
frame =
  surround "51" "54"
{-# INLINE frame #-}

-- | Encircle style.
encircle :: Builder -> Builder
encircle =
  surround "52" "54"
{-# INLINE encircle #-}

-- | O̅v̅e̅r̅l̅i̅n̅e̅ style.
overline :: Builder -> Builder
overline =
  surround "53" "55"
{-# INLINE overline #-}

--------------------------------------------------------------------------------

surround :: Builder -> Builder -> Builder -> Builder
surround open close text
  | isatty = esc <> open <> m <> text <> esc <> close <> m
  | otherwise = text
-- Don't inline before phase 1
{-# NOINLINE [1] surround #-}

esc :: Builder
esc =
  "\ESC["

m :: Builder
m =
  Builder.singleton 'm'

semi :: Builder
semi =
  Builder.singleton ';'

isatty :: Bool
isatty =
  unsafePerformIO (c_isatty 1) == 1
{-# NOINLINE isatty #-}

foreign import ccall unsafe "isatty"
  c_isatty :: CInt -> IO CInt

-- Collapse surround/surround to a single surround before phase 1
{-# RULES
"surround/surround" [~1] forall a b c d s.
  surround a b (surround c d s) =
    surround (a <> semi <> c) (b <> semi <> d) s
  #-}
