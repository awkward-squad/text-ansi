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
  surround "30" "39"
{-# INLINEABLE black #-}

-- | Red foreground.
red :: Text -> Text
red =
  surround "31" "39"
{-# INLINEABLE red #-}

-- | Green foreground.
green :: Text -> Text
green =
  surround "32" "39"
{-# INLINEABLE green #-}

-- | Yellow foreground.
yellow :: Text -> Text
yellow =
  surround "33" "39"
{-# INLINEABLE yellow #-}

-- | Blue foreground.
blue :: Text -> Text
blue =
  surround "34" "39"
{-# INLINEABLE blue #-}

-- | Magenta foreground.
magenta :: Text -> Text
magenta =
  surround "35" "39"
{-# INLINEABLE magenta #-}

-- | Cyan foreground.
cyan :: Text -> Text
cyan =
  surround "36" "39"
{-# INLINEABLE cyan #-}

-- | White foreground.
white :: Text -> Text
white =
  surround "37" "39"
{-# INLINEABLE white #-}

-- | Bright black foreground.
brightBlack :: Text -> Text
brightBlack =
  surround "90" "39"
{-# INLINEABLE brightBlack #-}

-- | Bright red foreground.
brightRed :: Text -> Text
brightRed =
  surround "91" "39"
{-# INLINEABLE brightRed #-}

-- | Bright green foreground.
brightGreen :: Text -> Text
brightGreen =
  surround "92" "39"
{-# INLINEABLE brightGreen #-}

-- | Bright yellow foreground.
brightYellow :: Text -> Text
brightYellow =
  surround "93" "39"
{-# INLINEABLE brightYellow #-}

-- | Bright blue foreground.
brightBlue :: Text -> Text
brightBlue =
  surround "94" "39"
{-# INLINEABLE brightBlue #-}

-- | Bright magenta foreground.
brightMagenta :: Text -> Text
brightMagenta =
  surround "95" "39"
{-# INLINEABLE brightMagenta #-}

-- | Bright cyan foreground.
brightCyan :: Text -> Text
brightCyan =
  surround "96" "39"
{-# INLINEABLE brightCyan #-}

-- | Bright white foreground.
brightWhite :: Text -> Text
brightWhite =
  surround "97" "39"
{-# INLINEABLE brightWhite #-}

-- | Black background.
blackBg :: Text -> Text
blackBg =
  surround "40" "49"
{-# INLINEABLE blackBg #-}

-- | Red background.
redBg :: Text -> Text
redBg =
  surround "41" "49"
{-# INLINEABLE redBg #-}

-- | Green background.
greenBg :: Text -> Text
greenBg =
  surround "42" "49"
{-# INLINEABLE greenBg #-}

-- | Yellow background.
yellowBg :: Text -> Text
yellowBg =
  surround "43" "49"
{-# INLINEABLE yellowBg #-}

-- | Blue background.
blueBg :: Text -> Text
blueBg =
  surround "44" "49"
{-# INLINEABLE blueBg #-}

-- | Magenta background.
magentaBg :: Text -> Text
magentaBg =
  surround "45" "49"
{-# INLINEABLE magentaBg #-}

-- | Cyan background.
cyanBg :: Text -> Text
cyanBg =
  surround "46" "49"
{-# INLINEABLE cyanBg #-}

-- | White background.
whiteBg :: Text -> Text
whiteBg =
  surround "47" "49"
{-# INLINEABLE whiteBg #-}

-- | Bright black background.
brightBlackBg :: Text -> Text
brightBlackBg =
  surround "100" "49"
{-# INLINEABLE brightBlackBg #-}

-- | Bright red background.
brightRedBg :: Text -> Text
brightRedBg =
  surround "101" "49"
{-# INLINEABLE brightRedBg #-}

-- | Bright green background.
brightGreenBg :: Text -> Text
brightGreenBg =
  surround "102" "49"
{-# INLINEABLE brightGreenBg #-}

-- | Bright yellow background.
brightYellowBg :: Text -> Text
brightYellowBg =
  surround "103" "49"
{-# INLINEABLE brightYellowBg #-}

-- | Bright blue background.
brightBlueBg :: Text -> Text
brightBlueBg =
  surround "104" "49"
{-# INLINEABLE brightBlueBg #-}

-- | Bright magenta background.
brightMagentaBg :: Text -> Text
brightMagentaBg =
  surround "105" "49"
{-# INLINEABLE brightMagentaBg #-}

-- | Bright cyan background.
brightCyanBg :: Text -> Text
brightCyanBg =
  surround "106" "49"
{-# INLINEABLE brightCyanBg #-}

-- | Bright white background.
brightWhiteBg :: Text -> Text
brightWhiteBg =
  surround "107" "49"
{-# INLINEABLE brightWhiteBg #-}

-- | RGB foreground.
rgb :: Word8 -> Word8 -> Word8 -> Text -> Text
rgb r g b =
  surround ("38;2;" <> Builder.decimal r <> semi <> Builder.decimal g <> semi <> Builder.decimal b) "39"
{-# INLINEABLE rgb #-}

-- | RGB background.
rgbBg :: Word8 -> Word8 -> Word8 -> Text -> Text
rgbBg r g b =
  surround ("48;2;" <> Builder.decimal r <> semi <> Builder.decimal g <> semi <> Builder.decimal b) "49"

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
  surround "3" "32"
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

-- Don't inline before phase 1
surround :: Builder -> Builder -> Text -> Text
surround open close text
  | isatty = Text.Lazy.toStrict (Builder.toLazyText (esc <> open <> m <> Builder.fromText text <> esc <> close <> m))
  | otherwise = text
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
