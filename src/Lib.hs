{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( runGame
    ) where

import           Control.Monad      (join, unless)
import           Data.Foldable
import           GHC.Word
import           Linear             (V4 (..))
import           SDL

import           Foreign.C.Types
import qualified SDL.Image
import           SDL.Video.Renderer

data Colour
  = Up Word8
  | Down Word8

data RGB =
  RGB { red   :: Colour
      , green :: Colour
      , blue  :: Colour
      }

cycleRGB :: RGB -> RGB
cycleRGB rgb
  = RGB { red   = cycleColour (red rgb)
        , green = cycleColour (green rgb)
        , blue  = cycleColour (blue rgb)
        }

invertRGB :: RGB -> RGB
invertRGB rgb
  = RGB { red = invertColour (red rgb)
        , green = invertColour (green rgb)
        , blue = invertColour (blue rgb)
        }

data Tile
  = Empty
  | Egg

type Grid = [[Tile]]

grid :: Grid
grid = [ [ Empty, Egg, Empty]
       , [ Egg, Empty, Egg ]
       , [ Empty, Egg, Empty ]
       ]

data RenderItem a
  = RenderItem
      { x     :: Int
      , y     :: Int
      , value :: a
      }

-- variant of map that passes each element's index as a second argument to f
mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

toRenderItems :: Grid -> [RenderItem Tile]
toRenderItems cols
  = join $ mapInd (\row -> \y -> mapInd (\item -> \x -> RenderItem x y item) row) cols

renderGrid :: Grid -> SDL.Texture -> SDL.Renderer -> IO ()
renderGrid grid tex renderer = do
  let drawTile (RenderItem x y v) =
        case v of
          Egg -> do
            let x' = fromIntegral (64 * x)
                y' = fromIntegral (64 * y)
            renderTexture renderer tex (P (V2 (CInt x') (CInt y')))
          _  -> pure ()
  traverse_ drawTile (toRenderItems grid)
  pure ()

blankRGB = RGB (Up 0) (Up 100) (Down 100)

invertColour :: Colour -> Colour
invertColour (Up i)   = Down (255 - i)
invertColour (Down i) = Up (255 - i)

cycleColour :: Colour -> Colour
cycleColour (Up i)
  = if i < 255
    then Up (i + 1)
    else Down 255
cycleColour (Down i)
  = if i > 0
    then Down (i - 1)
    else Up 0

getValue :: Colour -> Word8
getValue (Up i)   = i
getValue (Down i) = i

sampleRect :: Rectangle CInt
sampleRect
  = Rectangle (P (V2 (CInt 0) (CInt 0)))
              (V2 (CInt 100) (CInt 200))

type Position
  = Point V2 CInt

renderTexture :: SDL.Renderer -> SDL.Texture -> Position -> IO ()
renderTexture renderer tex pos = do
  ti <- SDL.queryTexture tex
  let (w, h) = (SDL.textureWidth ti, SDL.textureHeight ti)
      extent = V2 w h
  SDL.copy renderer tex Nothing (Just $ SDL.Rectangle pos extent)

runGame :: IO ()
runGame = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  background <- SDL.Image.loadTexture renderer "images/egg.png"
  appLoop blankRGB background renderer

appLoop :: RGB -> Texture -> Renderer -> IO ()
appLoop rgb texture renderer = do
  events <- pollEvents
  traverse print events
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  rendererDrawColor renderer $= V4 (getValue (red rgb))
                                   (getValue (green rgb))
                                   (getValue (blue rgb))
                                   100
  clear renderer
  let inverted = invertRGB rgb
  rendererDrawColor renderer $= V4 (getValue (red inverted))
                                   (getValue (green inverted))
                                   (getValue (blue inverted))
                                   100

  fillRect renderer (Just sampleRect)

  renderGrid grid texture renderer

  present renderer
  unless qPressed (appLoop (cycleRGB rgb) texture renderer)
