{-# LANGUAGE CPP #-}

module Text.Builder.ANSI
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
import Text.Builder (Builder)
import qualified Text.Builder as Builder
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
  foreground (Builder.string "30")
{-# INLINE black #-}

-- | Red foreground.
red :: Builder -> Builder
red =
  foreground (Builder.string "31")
{-# INLINE red #-}

-- | Green foreground.
green :: Builder -> Builder
green =
  foreground (Builder.string "32")
{-# INLINE green #-}

-- | Yellow foreground.
yellow :: Builder -> Builder
yellow =
  foreground (Builder.string "33")
{-# INLINE yellow #-}

-- | Blue foreground.
blue :: Builder -> Builder
blue =
  foreground (Builder.string "34")
{-# INLINE blue #-}

-- | Magenta foreground.
magenta :: Builder -> Builder
magenta =
  foreground (Builder.string "35")
{-# INLINE magenta #-}

-- | Cyan foreground.
cyan :: Builder -> Builder
cyan =
  foreground (Builder.string "36")
{-# INLINE cyan #-}

-- | White foreground.
white :: Builder -> Builder
white =
  foreground (Builder.string "37")
{-# INLINE white #-}

-- | Bright black foreground.
brightBlack :: Builder -> Builder
brightBlack =
  foreground (Builder.string "90")
{-# INLINE brightBlack #-}

-- | Bright red foreground.
brightRed :: Builder -> Builder
brightRed =
  foreground (Builder.string "91")
{-# INLINE brightRed #-}

-- | Bright green foreground.
brightGreen :: Builder -> Builder
brightGreen =
  foreground (Builder.string "92")
{-# INLINE brightGreen #-}

-- | Bright yellow foreground.
brightYellow :: Builder -> Builder
brightYellow =
  foreground (Builder.string "93")
{-# INLINE brightYellow #-}

-- | Bright blue foreground.
brightBlue :: Builder -> Builder
brightBlue =
  foreground (Builder.string "94")
{-# INLINE brightBlue #-}

-- | Bright magenta foreground.
brightMagenta :: Builder -> Builder
brightMagenta =
  foreground (Builder.string "95")
{-# INLINE brightMagenta #-}

-- | Bright cyan foreground.
brightCyan :: Builder -> Builder
brightCyan =
  foreground (Builder.string "96")
{-# INLINE brightCyan #-}

-- | Bright white foreground.
brightWhite :: Builder -> Builder
brightWhite =
  foreground (Builder.string "97")
{-# INLINE brightWhite #-}

-- | RGB foreground.
rgb :: Word8 -> Word8 -> Word8 -> Builder -> Builder
rgb r g b =
  foreground (Builder.string "38;2;" <> Builder.decimal r <> semi <> Builder.decimal g <> semi <> Builder.decimal b)
{-# INLINE rgb #-}

foreground :: Builder -> Builder -> Builder
foreground s =
  surround s (Builder.string "39")
{-# INLINE foreground #-}

-- | Black background.
blackBg :: Builder -> Builder
blackBg =
  background (Builder.string "40")
{-# INLINE blackBg #-}

-- | Red background.
redBg :: Builder -> Builder
redBg =
  background (Builder.string "41")
{-# INLINE redBg #-}

-- | Green background.
greenBg :: Builder -> Builder
greenBg =
  background (Builder.string "42")
{-# INLINE greenBg #-}

-- | Yellow background.
yellowBg :: Builder -> Builder
yellowBg =
  background (Builder.string "43")
{-# INLINE yellowBg #-}

-- | Blue background.
blueBg :: Builder -> Builder
blueBg =
  background (Builder.string "44")
{-# INLINE blueBg #-}

-- | Magenta background.
magentaBg :: Builder -> Builder
magentaBg =
  background (Builder.string "45")
{-# INLINE magentaBg #-}

-- | Cyan background.
cyanBg :: Builder -> Builder
cyanBg =
  background (Builder.string "46")
{-# INLINE cyanBg #-}

-- | White background.
whiteBg :: Builder -> Builder
whiteBg =
  background (Builder.string "47")
{-# INLINE whiteBg #-}

-- | Bright black background.
brightBlackBg :: Builder -> Builder
brightBlackBg =
  background (Builder.string "100")
{-# INLINE brightBlackBg #-}

-- | Bright red background.
brightRedBg :: Builder -> Builder
brightRedBg =
  background (Builder.string "101")
{-# INLINE brightRedBg #-}

-- | Bright green background.
brightGreenBg :: Builder -> Builder
brightGreenBg =
  background (Builder.string "102")
{-# INLINE brightGreenBg #-}

-- | Bright yellow background.
brightYellowBg :: Builder -> Builder
brightYellowBg =
  background (Builder.string "103")
{-# INLINE brightYellowBg #-}

-- | Bright blue background.
brightBlueBg :: Builder -> Builder
brightBlueBg =
  background (Builder.string "104")
{-# INLINE brightBlueBg #-}

-- | Bright magenta background.
brightMagentaBg :: Builder -> Builder
brightMagentaBg =
  background (Builder.string "105")
{-# INLINE brightMagentaBg #-}

-- | Bright cyan background.
brightCyanBg :: Builder -> Builder
brightCyanBg =
  background (Builder.string "106")
{-# INLINE brightCyanBg #-}

-- | Bright white background.
brightWhiteBg :: Builder -> Builder
brightWhiteBg =
  background (Builder.string "107")
{-# INLINE brightWhiteBg #-}

background :: Builder -> Builder -> Builder
background s =
  surround s (Builder.string "49")
{-# INLINE background #-}

-- | RGB background.
rgbBg :: Word8 -> Word8 -> Word8 -> Builder -> Builder
rgbBg r g b =
  background (Builder.string "48;2;" <> Builder.decimal r <> semi <> Builder.decimal g <> semi <> Builder.decimal b)
{-# INLINE rgbBg #-}

-- | __Bold__ style (high intensity).
bold :: Builder -> Builder
bold =
  surround (Builder.string "1") (Builder.string "22")
{-# INLINE bold #-}

-- | Faint style (low intensity).
faint :: Builder -> Builder
faint =
  surround (Builder.string "2") (Builder.string "22")
{-# INLINE faint #-}

-- | /Italic/ style.
italic :: Builder -> Builder
italic =
  surround (Builder.string "3") (Builder.string "23")
{-# INLINE italic #-}

-- | U̲n̲d̲e̲r̲l̲i̲n̲e̲ style.
underline :: Builder -> Builder
underline =
  surround (Builder.string "4") (Builder.string "24")
{-# INLINE underline #-}

-- | D̳o̳u̳b̳l̳e̳ ̳u̳n̳d̳e̳r̳l̳i̳n̳e̳ style.
doubleUnderline :: Builder -> Builder
doubleUnderline =
  surround (Builder.string "21") (Builder.string "24")
{-# INLINE doubleUnderline #-}

-- | S̶t̶r̶i̶k̶e̶t̶h̶r̶o̶u̶g̶h̶ style.
strikethrough :: Builder -> Builder
strikethrough =
  surround (Builder.string "9") (Builder.string "29")
{-# INLINE strikethrough #-}

-- | Frame style.
frame :: Builder -> Builder
frame =
  surround (Builder.string "51") (Builder.string "54")
{-# INLINE frame #-}

-- | Encircle style.
encircle :: Builder -> Builder
encircle =
  surround (Builder.string "52") (Builder.string "54")
{-# INLINE encircle #-}

-- | O̅v̅e̅r̅l̅i̅n̅e̅ style.
overline :: Builder -> Builder
overline =
  surround (Builder.string "53") (Builder.string "55")
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
  Builder.string "\ESC["

m :: Builder
m =
  Builder.char 'm'

semi :: Builder
semi =
  Builder.char ';'

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
