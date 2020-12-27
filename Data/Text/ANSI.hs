{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Text.ANSI
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
import Data.Text
import qualified Data.Text.Lazy as Text.Lazy
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
black :: Text -> Text
black =
  foreground "30"
{-# INLINEABLE black #-}

-- | Red foreground.
red :: Text -> Text
red =
  foreground "31"
{-# INLINEABLE red #-}

-- | Green foreground.
green :: Text -> Text
green =
  foreground "32"
{-# INLINEABLE green #-}

-- | Yellow foreground.
yellow :: Text -> Text
yellow =
  foreground "33"
{-# INLINEABLE yellow #-}

-- | Blue foreground.
blue :: Text -> Text
blue =
  foreground "34"
{-# INLINEABLE blue #-}

-- | Magenta foreground.
magenta :: Text -> Text
magenta =
  foreground "35"
{-# INLINEABLE magenta #-}

-- | Cyan foreground.
cyan :: Text -> Text
cyan =
  foreground "36"
{-# INLINEABLE cyan #-}

-- | White foreground.
white :: Text -> Text
white =
  foreground "37"
{-# INLINEABLE white #-}

-- | Bright black foreground.
brightBlack :: Text -> Text
brightBlack =
  foreground "90"
{-# INLINEABLE brightBlack #-}

-- | Bright red foreground.
brightRed :: Text -> Text
brightRed =
  foreground "91"
{-# INLINEABLE brightRed #-}

-- | Bright green foreground.
brightGreen :: Text -> Text
brightGreen =
  foreground "92"
{-# INLINEABLE brightGreen #-}

-- | Bright yellow foreground.
brightYellow :: Text -> Text
brightYellow =
  foreground "93"
{-# INLINEABLE brightYellow #-}

-- | Bright blue foreground.
brightBlue :: Text -> Text
brightBlue =
  foreground "94"
{-# INLINEABLE brightBlue #-}

-- | Bright magenta foreground.
brightMagenta :: Text -> Text
brightMagenta =
  foreground "95"
{-# INLINEABLE brightMagenta #-}

-- | Bright cyan foreground.
brightCyan :: Text -> Text
brightCyan =
  foreground "96"
{-# INLINEABLE brightCyan #-}

-- | Bright white foreground.
brightWhite :: Text -> Text
brightWhite =
  foreground "97"
{-# INLINEABLE brightWhite #-}

-- | RGB foreground.
rgb :: Word8 -> Word8 -> Word8 -> Text -> Text
rgb r g b =
  foreground ("38;2;" <> Builder.decimal r <> semi <> Builder.decimal g <> semi <> Builder.decimal b)
{-# INLINEABLE rgb #-}

foreground :: Builder -> Text -> Text
foreground s =
  surround s "39"
{-# INLINE foreground #-}

-- | Black background.
blackBg :: Text -> Text
blackBg =
  background "40"
{-# INLINEABLE blackBg #-}

-- | Red background.
redBg :: Text -> Text
redBg =
  background "41"
{-# INLINEABLE redBg #-}

-- | Green background.
greenBg :: Text -> Text
greenBg =
  background "42"
{-# INLINEABLE greenBg #-}

-- | Yellow background.
yellowBg :: Text -> Text
yellowBg =
  background "43"
{-# INLINEABLE yellowBg #-}

-- | Blue background.
blueBg :: Text -> Text
blueBg =
  background "44"
{-# INLINEABLE blueBg #-}

-- | Magenta background.
magentaBg :: Text -> Text
magentaBg =
  background "45"
{-# INLINEABLE magentaBg #-}

-- | Cyan background.
cyanBg :: Text -> Text
cyanBg =
  background "46"
{-# INLINEABLE cyanBg #-}

-- | White background.
whiteBg :: Text -> Text
whiteBg =
  background "47"
{-# INLINEABLE whiteBg #-}

-- | Bright black background.
brightBlackBg :: Text -> Text
brightBlackBg =
  background "100"
{-# INLINEABLE brightBlackBg #-}

-- | Bright red background.
brightRedBg :: Text -> Text
brightRedBg =
  background "101"
{-# INLINEABLE brightRedBg #-}

-- | Bright green background.
brightGreenBg :: Text -> Text
brightGreenBg =
  background "102"
{-# INLINEABLE brightGreenBg #-}

-- | Bright yellow background.
brightYellowBg :: Text -> Text
brightYellowBg =
  background "103"
{-# INLINEABLE brightYellowBg #-}

-- | Bright blue background.
brightBlueBg :: Text -> Text
brightBlueBg =
  background "104"
{-# INLINEABLE brightBlueBg #-}

-- | Bright magenta background.
brightMagentaBg :: Text -> Text
brightMagentaBg =
  background "105"
{-# INLINEABLE brightMagentaBg #-}

-- | Bright cyan background.
brightCyanBg :: Text -> Text
brightCyanBg =
  background "106"
{-# INLINEABLE brightCyanBg #-}

-- | Bright white background.
brightWhiteBg :: Text -> Text
brightWhiteBg =
  background "107"
{-# INLINEABLE brightWhiteBg #-}

background :: Builder -> Text -> Text
background s =
  surround s "49"
{-# INLINE background #-}

-- | RGB background.
rgbBg :: Word8 -> Word8 -> Word8 -> Text -> Text
rgbBg r g b =
  background ("48;2;" <> Builder.decimal r <> semi <> Builder.decimal g <> semi <> Builder.decimal b)
{-# INLINEABLE rgbBg #-}

-- | __Bold__ style (high intensity).
bold :: Text -> Text
bold =
  surround "1" "22"
{-# INLINEABLE bold #-}

-- | Faint style (low intensity).
faint :: Text -> Text
faint =
  surround "2" "22"
{-# INLINEABLE faint #-}

-- | /Italic/ style.
italic :: Text -> Text
italic =
  surround "3" "23"
{-# INLINEABLE italic #-}

-- | U̲n̲d̲e̲r̲l̲i̲n̲e̲ style.
underline :: Text -> Text
underline =
  surround "4" "24"
{-# INLINEABLE underline #-}

-- | D̳o̳u̳b̳l̳e̳ ̳u̳n̳d̳e̳r̳l̳i̳n̳e̳ style.
doubleUnderline :: Text -> Text
doubleUnderline =
  surround "21" "24"
{-# INLINEABLE doubleUnderline #-}

-- | S̶t̶r̶i̶k̶e̶t̶h̶r̶o̶u̶g̶h̶ style.
strikethrough :: Text -> Text
strikethrough =
  surround "9" "29"
{-# INLINEABLE strikethrough #-}

-- | Frame style.
frame :: Text -> Text
frame =
  surround "51" "54"
{-# INLINEABLE frame #-}

-- | Encircle style.
encircle :: Text -> Text
encircle =
  surround "52" "54"
{-# INLINEABLE encircle #-}

-- | O̅v̅e̅r̅l̅i̅n̅e̅ style.
overline :: Text -> Text
overline =
  surround "53" "55"
{-# INLINEABLE overline #-}

--------------------------------------------------------------------------------

surround :: Builder -> Builder -> Text -> Text
surround open close text
  | isatty = Text.Lazy.toStrict (Builder.toLazyText (esc <> open <> m <> Builder.fromText text <> esc <> close <> m))
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
