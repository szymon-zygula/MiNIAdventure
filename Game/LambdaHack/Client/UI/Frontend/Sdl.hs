-- | Text frontend based on SDL2.
module Game.LambdaHack.Client.UI.Frontend.Sdl
  ( startup, frontendName
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , startupFun, shutdown, display
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude hiding (Alt)

import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import Data.IORef
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32, Word8)
import Foreign.C.Types (CInt)
import System.Directory
import System.Exit (exitSuccess)
import System.FilePath

import qualified SDL
import qualified SDL.Font as TTF
import SDL.Input.Keyboard.Codes
import qualified SDL.Vect as Vect

import Game.LambdaHack.Client.UI.Frame
import Game.LambdaHack.Client.UI.Frontend.Common
import qualified Game.LambdaHack.Client.UI.Key as K
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray

type FontAtlas = EM.EnumMap Color.AttrCharW32 SDL.Texture

-- | Session data maintained by the frontend.
data FrontendSession = FrontendSession
  { swindow           :: !SDL.Window
  , srenderer         :: !SDL.Renderer
  , sfont             :: !TTF.Font
  , satlas            :: !(IORef FontAtlas)
  , stexture          :: !(IORef SDL.Texture)
  , spreviousFrame    :: !(IORef SingleFrame)
  , sforcedShutdown   :: !(IORef Bool)
  , sdisplayPermitted :: !(MVar Bool)
  }

-- | The name of the frontend.
frontendName :: String
frontendName = "sdl"

-- | Set up and start the main loop providing input and output.
--
-- Apparently some SDL backends are not thread-safe
-- (https://wiki.libsdl.org/FAQDevelopment),
-- so we stick to main thread.
startup :: DebugModeCli -> IO RawFrontend
startup sdebugCli = startupBound $ startupFun sdebugCli

startupFun :: DebugModeCli -> MVar RawFrontend -> IO ()
startupFun sdebugCli@DebugModeCli{..} rfMVar = do
  SDL.initialize [SDL.InitVideo, SDL.InitEvents]
  let title = fromJust stitle
      fontFileName = T.unpack (fromJust sdlFontFile)
      fontFile | isRelative fontFileName = fromJust sfontDir </> fontFileName
               | otherwise = fontFileName
  fontFileExists <- doesFileExist fontFile
  unless fontFileExists $
    fail $ "Font file does not exist: " ++ fontFile
  let fontSize = fromJust sfontSize
  TTF.initialize
  sfont <- TTF.load fontFile fontSize
  let isFonFile = "fon" `isSuffixOf` T.unpack (fromJust sdlFontFile)
      sdlSizeAdd = fromJust $ if isFonFile then sdlFonSizeAdd else sdlTtfSizeAdd
  boxSize <- (+ sdlSizeAdd) <$> TTF.height sfont
  let xsize = fst normalLevelBound + 1
      ysize = snd normalLevelBound + 4
      screenV2 = SDL.V2 (toEnum $ xsize * boxSize)
                        (toEnum $ ysize * boxSize)
      windowConfig = SDL.defaultWindow {SDL.windowInitialSize = screenV2}
      rendererConfig = SDL.defaultRenderer {SDL.rendererTargetTexture = True}
  swindow <- SDL.createWindow title windowConfig
  srenderer <- SDL.createRenderer swindow (-1) rendererConfig
  let initTexture = do
        texture <- SDL.createTexture srenderer SDL.ARGB8888
                                     SDL.TextureAccessTarget screenV2
        SDL.rendererRenderTarget srenderer SDL.$= Just texture
        SDL.rendererDrawBlendMode srenderer SDL.$= SDL.BlendNone
        SDL.rendererDrawColor srenderer SDL.$= colorToRGBA Color.Black
        SDL.clear srenderer  -- clear the texture
        SDL.rendererRenderTarget srenderer SDL.$= Nothing
        SDL.copy srenderer texture Nothing Nothing  -- clear the backbuffer
        return texture
  texture <- initTexture
  satlas <- newIORef EM.empty
  stexture <- newIORef texture
  spreviousFrame <- newIORef blankSingleFrame
  sforcedShutdown <- newIORef False
  sdisplayPermitted <- newMVar True
  let sess = FrontendSession{..}
  rf <- createRawFrontend (display sdebugCli sess) (shutdown sess)
  putMVar rfMVar rf
  let pointTranslate :: forall i. (Enum i) => Vect.Point Vect.V2 i -> Point
      pointTranslate (SDL.P (SDL.V2 x y)) =
        Point (fromEnum x `div` boxSize) (fromEnum y `div` boxSize)
      redraw = do
        displayPermitted <- takeMVar sdisplayPermitted
        when displayPermitted $ do
          -- Textures may be trashed and even invalid, especially on Windows.
          atlas <- readIORef satlas
          writeIORef satlas EM.empty
          oldTexture <- readIORef stexture
          newTexture <- initTexture
          mapM_ SDL.destroyTexture $ EM.elems atlas
          SDL.destroyTexture oldTexture
          writeIORef stexture newTexture
          prevFrame <- readIORef spreviousFrame
          writeIORef spreviousFrame blankSingleFrame
          displayNoLock sdebugCli sess prevFrame
        putMVar sdisplayPermitted displayPermitted
      storeKeys :: IO ()
      storeKeys = do
        e <- SDL.waitEvent  -- blocks here, so no polling
        case SDL.eventPayload e of
          SDL.KeyboardEvent keyboardEvent
            | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed -> do
              let sym = SDL.keyboardEventKeysym keyboardEvent
                  ksm = SDL.keysymModifier sym
                  shiftPressed = SDL.keyModifierLeftShift ksm
                                 || SDL.keyModifierRightShift ksm
                  key = keyTranslate shiftPressed $ SDL.keysymKeycode sym
                  modifier = modTranslate ksm
              p <- SDL.getAbsoluteMouseLocation
              when (key == K.Esc) $ resetChanKey (fchanKey rf)
              saveKMP rf modifier key (pointTranslate p)
          SDL.MouseButtonEvent mouseButtonEvent
            | SDL.mouseButtonEventMotion mouseButtonEvent == SDL.Released -> do
              md <- modTranslate <$> SDL.getModState
              let key = case SDL.mouseButtonEventButton mouseButtonEvent of
                    SDL.ButtonLeft -> K.LeftButtonRelease
                    SDL.ButtonMiddle -> K.MiddleButtonRelease
                    SDL.ButtonRight -> K.RightButtonRelease
                    _ -> K.LeftButtonRelease  -- any other is spare left
                  modifier = if md == K.Shift then K.NoModifier else md
                  p = SDL.mouseButtonEventPos mouseButtonEvent
              saveKMP rf modifier key (pointTranslate p)
          SDL.MouseWheelEvent mouseWheelEvent -> do
            md <- modTranslate <$> SDL.getModState
            let SDL.V2 _ y = SDL.mouseWheelEventPos mouseWheelEvent
                mkey = case (compare y 0, SDL.mouseWheelEventDirection
                                            mouseWheelEvent) of
                  (EQ, _) -> Nothing
                  (LT, SDL.ScrollNormal) -> Just K.WheelSouth
                  (GT, SDL.ScrollNormal) -> Just K.WheelNorth
                  (LT, SDL.ScrollFlipped) -> Just K.WheelSouth
                  (GT, SDL.ScrollFlipped) -> Just K.WheelNorth
                modifier = if md == K.Shift then K.NoModifier else md
            p <- SDL.getAbsoluteMouseLocation
            maybe (return ())
                  (\key -> saveKMP rf modifier key (pointTranslate p)) mkey
          SDL.WindowClosedEvent{} -> forceShutdown sess
          SDL.QuitEvent -> forceShutdown sess
          SDL.WindowRestoredEvent{} -> redraw
          SDL.WindowExposedEvent{} -> redraw  -- needed on Windows
          -- Probably not needed, because textures nor their content not lost:
          -- SDL.WindowShownEvent{} -> redraw
          _ -> return ()
        displayPermitted <- readMVar sdisplayPermitted
        if displayPermitted
        then storeKeys
        else do
          TTF.free sfont
          TTF.quit
          SDL.destroyRenderer srenderer
          SDL.destroyWindow swindow
          SDL.quit
          forcedShutdown <- readIORef sforcedShutdown
          when forcedShutdown
            exitSuccess  -- not in the main thread, so no exit yet, see "Main"
  storeKeys

shutdown :: FrontendSession -> IO ()
shutdown FrontendSession{..} = void $ swapMVar sdisplayPermitted False

forceShutdown :: FrontendSession -> IO ()
forceShutdown sess@FrontendSession{..} = do
  writeIORef sforcedShutdown True
  shutdown sess

-- | Add a frame to be drawn.
display :: DebugModeCli
        -> FrontendSession  -- ^ frontend session data
        -> SingleFrame      -- ^ the screen frame to draw
        -> IO ()
display sdebugCli sess@FrontendSession{..} curFrame = do
  displayPermitted <- takeMVar sdisplayPermitted
  if displayPermitted then do
    -- Apparently some SDL backends are not thread-safe, so keep to main thread:
    a <- asyncBound $ displayNoLock sdebugCli sess curFrame
    wait a
    putMVar sdisplayPermitted displayPermitted
  else do
    putMVar sdisplayPermitted displayPermitted
    forcedShutdown <- readIORef sforcedShutdown
    when forcedShutdown $
      -- When there's a forced shutdown, ignore displaying one frame
      -- and don't occupy the CPU creating new ones and moving on with the game
      -- (possibly also saving the new game state, surprising the player),
      -- but give time for SDL to clean up and exit via @exitSuccess@
      -- to avoid exiting via "thread blocked".
      threadDelay 50000

displayNoLock :: DebugModeCli
              -> FrontendSession  -- ^ frontend session data
              -> SingleFrame      -- ^ the screen frame to draw
              -> IO ()
displayNoLock DebugModeCli{..} FrontendSession{..} curFrame = do
  let isFonFile = "fon" `isSuffixOf` T.unpack (fromJust sdlFontFile)
      sdlSizeAdd = fromJust $ if isFonFile then sdlFonSizeAdd else sdlTtfSizeAdd
  boxSize <- (+ sdlSizeAdd) <$> TTF.height sfont
  let xsize = fst normalLevelBound + 1
      vp :: Int -> Int -> Vect.Point Vect.V2 CInt
      vp x y = Vect.P $ Vect.V2 (toEnum x) (toEnum y)
      drawHighlight x y color = do
        SDL.rendererDrawColor srenderer SDL.$= colorToRGBA color
        let rect = SDL.Rectangle (vp (x * boxSize) (y * boxSize))
                                 (Vect.V2 (toEnum boxSize) (toEnum boxSize))
        SDL.drawRect srenderer $ Just rect
        SDL.rendererDrawColor srenderer SDL.$= colorToRGBA Color.Black  -- reset back to black
      setChar :: Int -> Word32 -> Word32 -> IO ()
      setChar i w wPrev = unless (w == wPrev) $ do
        atlas <- readIORef satlas
        let (y, x) = i `divMod` xsize
            acRaw = Color.AttrCharW32 w
            Color.AttrChar{acAttr=Color.Attr{..}, acChar=acCharRaw} =
              Color.attrCharFromW32 acRaw
            normalizeAc color = (Color.attrChar2ToW32 fg acCharRaw, Just color)
            (ac, mlineColor) = case bg of
              Color.HighlightNone -> (acRaw, Nothing)
              Color.HighlightRed -> normalizeAc Color.Red
              Color.HighlightBlue -> normalizeAc Color.Blue
              Color.HighlightYellow -> normalizeAc Color.BrYellow
              Color.HighlightGrey -> normalizeAc Color.BrBlack
        -- https://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf_42.html#SEC42
        textTexture <- case EM.lookup ac atlas of
          Nothing -> do
            -- Make all visible floors bold (no bold fold variant for 16x16x,
            -- so only the dot can be bold).
            let acChar = if fg <= Color.BrBlack
                            && Char.ord acCharRaw == 183  -- 0xb7
                            && scolorIsBold == Just True  -- only dot but enough
                         then Char.chr $ if isFonFile
                                         then 7   -- hack
                                         else 8901  -- 0x22c5
                         else acCharRaw
            textSurface <-
              TTF.shadedGlyph sfont (colorToRGBA fg) (colorToRGBA Color.Black) acChar
            textTexture <- SDL.createTextureFromSurface srenderer textSurface
            SDL.freeSurface textSurface
            writeIORef satlas $ EM.insert ac textTexture atlas  -- not @acRaw@
            return textTexture
          Just textTexture -> return textTexture
        ti <- SDL.queryTexture textTexture
        let box = SDL.Rectangle (vp (x * boxSize) (y * boxSize))
                                (Vect.V2 (toEnum boxSize) (toEnum boxSize))
            width = min boxSize $ fromEnum $ SDL.textureWidth ti
            height = min boxSize $ fromEnum $ SDL.textureHeight ti
            xsrc = max 0 (fromEnum (SDL.textureWidth ti) - width) `div` 2
            ysrc = max 0 (fromEnum (SDL.textureHeight ti) - height) `div` 2
            srcR = SDL.Rectangle (vp xsrc ysrc)
                                 (Vect.V2 (toEnum width) (toEnum height))
            xtgt = (boxSize - width) `divUp` 2
            ytgt = (boxSize - height) `div` 2
            tgtR = SDL.Rectangle (vp (x * boxSize + xtgt) (y * boxSize + ytgt))
                                 (Vect.V2 (toEnum width) (toEnum height))
        SDL.fillRect srenderer $ Just box
        SDL.copy srenderer textTexture (Just srcR) (Just tgtR)
        maybe (return ()) (drawHighlight x y) mlineColor
  texture <- readIORef stexture
  prevFrame <- readIORef spreviousFrame
  writeIORef spreviousFrame curFrame
  SDL.rendererRenderTarget srenderer SDL.$= Just texture
  SDL.rendererDrawColor srenderer SDL.$= colorToRGBA Color.Black
  U.izipWithM_ setChar (PointArray.avector $ singleFrame curFrame)
                       (PointArray.avector $ singleFrame prevFrame)
  SDL.rendererRenderTarget srenderer SDL.$= Nothing
  SDL.copy srenderer texture Nothing Nothing  -- clear the backbuffer
  SDL.present srenderer

-- | Translates modifiers to our own encoding, ignoring Shift.
modTranslate :: SDL.KeyModifier -> K.Modifier
modTranslate m =
  modifierTranslate
    (SDL.keyModifierLeftCtrl m || SDL.keyModifierRightCtrl m)
    False
    (SDL.keyModifierLeftAlt m
     || SDL.keyModifierRightAlt m
     || SDL.keyModifierAltGr m)
    False

keyTranslate :: Bool -> SDL.Keycode -> K.Key
keyTranslate shiftPressed n =
  case n of
    KeycodeEscape     -> K.Esc
    KeycodeReturn     -> K.Return
    KeycodeBackspace  -> K.BackSpace
    KeycodeTab        -> if shiftPressed then K.BackTab else K.Tab
    KeycodeSpace      -> K.Space
    KeycodeExclaim -> K.Char '!'
    KeycodeQuoteDbl -> K.Char '"'
    KeycodeHash -> K.Char '#'
    KeycodePercent -> K.Char '%'
    KeycodeDollar -> K.Char '$'
    KeycodeAmpersand -> K.Char '&'
    KeycodeQuote -> if shiftPressed then K.Char '"' else K.Char '\''
    KeycodeLeftParen -> K.Char '('
    KeycodeRightParen -> K.Char ')'
    KeycodeAsterisk -> K.Char '*'
    KeycodePlus -> K.Char '+'
    KeycodeComma -> if shiftPressed then K.Char '<' else K.Char ','
    KeycodeMinus -> if shiftPressed then K.Char '_' else K.Char '-'
    KeycodePeriod -> if shiftPressed then K.Char '>' else K.Char '.'
    KeycodeSlash -> if shiftPressed then K.Char '?' else K.Char '/'
    Keycode1 -> if shiftPressed then K.Char '!' else K.Char '1'
    Keycode2 -> if shiftPressed then K.Char '@' else K.Char '2'
    Keycode3 -> if shiftPressed then K.Char '#' else K.Char '3'
    Keycode4 -> if shiftPressed then K.Char '$' else K.Char '4'
    Keycode5 -> if shiftPressed then K.Char '%' else K.Char '5'
    Keycode6 -> if shiftPressed then K.Char '^' else K.Char '6'
    Keycode7 -> if shiftPressed then K.Char '&' else K.Char '7'
    Keycode8 -> if shiftPressed then K.Char '*' else K.Char '8'
    Keycode9 -> if shiftPressed then K.Char '(' else K.Char '9'
    Keycode0 -> if shiftPressed then K.Char ')' else K.Char '0'
    KeycodeColon -> K.Char ':'
    KeycodeSemicolon -> if shiftPressed then K.Char ':' else K.Char ';'
    KeycodeLess -> K.Char '<'
    KeycodeEquals -> if shiftPressed then K.Char '+' else K.Char '='
    KeycodeGreater -> K.Char '>'
    KeycodeQuestion -> K.Char '?'
    KeycodeAt -> K.Char '@'
    KeycodeLeftBracket -> if shiftPressed then K.Char '{' else K.Char '['
    KeycodeBackslash -> if shiftPressed then K.Char '|' else K.Char '\\'
    KeycodeRightBracket -> if shiftPressed then K.Char '}' else K.Char ']'
    KeycodeCaret -> K.Char '^'
    KeycodeUnderscore -> K.Char '_'
    KeycodeBackquote -> if shiftPressed then K.Char '~' else K.Char '`'
    KeycodeUp         -> K.Up
    KeycodeDown       -> K.Down
    KeycodeLeft       -> K.Left
    KeycodeRight      -> K.Right
    KeycodeHome       -> K.Home
    KeycodeEnd        -> K.End
    KeycodePageUp     -> K.PgUp
    KeycodePageDown   -> K.PgDn
    KeycodeInsert     -> K.Insert
    KeycodeDelete     -> K.Delete
    KeycodeKPDivide   -> K.KP '/'
    KeycodeKPMultiply -> K.KP '*'
    KeycodeKPMinus    -> K.Char '-'  -- KP and normal are merged here
    KeycodeKPPlus     -> K.Char '+'  -- KP and normal are merged here
    KeycodeKPEnter    -> K.Return
    KeycodeKPEquals   -> K.Return  -- in case of some funny layouts
    KeycodeKP1 -> if shiftPressed then K.KP '1' else K.End
    KeycodeKP2 -> if shiftPressed then K.KP '2' else K.Down
    KeycodeKP3 -> if shiftPressed then K.KP '3' else K.PgDn
    KeycodeKP4 -> if shiftPressed then K.KP '4' else K.Left
    KeycodeKP5 -> if shiftPressed then K.KP '5' else K.Begin
    KeycodeKP6 -> if shiftPressed then K.KP '6' else K.Right
    KeycodeKP7 -> if shiftPressed then K.KP '7' else K.Home
    KeycodeKP8 -> if shiftPressed then K.KP '8' else K.Up
    KeycodeKP9 -> if shiftPressed then K.KP '9' else K.PgUp
    KeycodeKP0 -> if shiftPressed then K.KP '0' else K.Insert
    KeycodeKPPeriod -> K.Char '.'  -- dot and comma are merged here
    KeycodeKPComma  -> K.Char '.'  -- to sidestep national standards
    KeycodeF1       -> K.Fun 1
    KeycodeF2       -> K.Fun 2
    KeycodeF3       -> K.Fun 3
    KeycodeF4       -> K.Fun 4
    KeycodeF5       -> K.Fun 5
    KeycodeF6       -> K.Fun 6
    KeycodeF7       -> K.Fun 7
    KeycodeF8       -> K.Fun 8
    KeycodeF9       -> K.Fun 9
    KeycodeF10      -> K.Fun 10
    KeycodeF11      -> K.Fun 11
    KeycodeF12      -> K.Fun 12
    KeycodeLCtrl    -> K.DeadKey
    KeycodeLShift   -> K.DeadKey
    KeycodeLAlt     -> K.DeadKey
    KeycodeLGUI     -> K.DeadKey
    KeycodeRCtrl    -> K.DeadKey
    KeycodeRShift   -> K.DeadKey
    KeycodeRAlt     -> K.DeadKey
    KeycodeRGUI     -> K.DeadKey
    KeycodeMode     -> K.DeadKey
    KeycodeNumLockClear -> K.DeadKey
    KeycodeUnknown  -> K.Unknown "KeycodeUnknown"
    _ -> let i = fromEnum $ unwrapKeycode n
         in if | 97 <= i && i <= 122
                 && shiftPressed -> K.Char $ Char.chr $ i - 32
               | 32 <= i && i <= 126 -> K.Char $ Char.chr i
               | otherwise -> K.Unknown $ show n


sDL_ALPHA_OPAQUE :: Word8
sDL_ALPHA_OPAQUE = 255

-- This code is sadly duplicated from "Game.LambdaHack.Common.Color".
colorToRGBA :: Color.Color -> SDL.V4 Word8
colorToRGBA Color.Black     = SDL.V4 0 0 0 sDL_ALPHA_OPAQUE
colorToRGBA Color.Red       = SDL.V4 0xD5 0x00 0x00 sDL_ALPHA_OPAQUE
colorToRGBA Color.Green     = SDL.V4 0x00 0xAA 0x00 sDL_ALPHA_OPAQUE
colorToRGBA Color.Brown     = SDL.V4 0xCA 0x4A 0x00 sDL_ALPHA_OPAQUE
colorToRGBA Color.Blue      = SDL.V4 0x20 0x3A 0xF0 sDL_ALPHA_OPAQUE
colorToRGBA Color.Magenta   = SDL.V4 0xAA 0x00 0xAA sDL_ALPHA_OPAQUE
colorToRGBA Color.Cyan      = SDL.V4 0x00 0xAA 0xAA sDL_ALPHA_OPAQUE
colorToRGBA Color.White     = SDL.V4 0xC5 0xBC 0xB8 sDL_ALPHA_OPAQUE
colorToRGBA Color.BrBlack   = SDL.V4 0x6F 0x5F 0x5F sDL_ALPHA_OPAQUE
colorToRGBA Color.BrRed     = SDL.V4 0xFF 0x55 0x55 sDL_ALPHA_OPAQUE
colorToRGBA Color.BrGreen   = SDL.V4 0x75 0xFF 0x45 sDL_ALPHA_OPAQUE
colorToRGBA Color.BrYellow  = SDL.V4 0xFF 0xE8 0x55 sDL_ALPHA_OPAQUE
colorToRGBA Color.BrBlue    = SDL.V4 0x40 0x90 0xFF sDL_ALPHA_OPAQUE
colorToRGBA Color.BrMagenta = SDL.V4 0xFF 0x77 0xFF sDL_ALPHA_OPAQUE
colorToRGBA Color.BrCyan    = SDL.V4 0x60 0xFF 0xF0 sDL_ALPHA_OPAQUE
colorToRGBA Color.BrWhite   = SDL.V4 0xFF 0xFF 0xFF sDL_ALPHA_OPAQUE
