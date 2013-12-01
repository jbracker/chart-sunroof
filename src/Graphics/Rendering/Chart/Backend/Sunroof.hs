
-- | Backend for the Chart that renders a chart as Javascript 
--   rendering operations on a HTML5 canvas. 
module Graphics.Rendering.Chart.Backend.Sunroof
  (
  , runBackend
  , defaultEnv
  
  ) where

import Data.Colour ( AlphaColour, toSRGB, colourChannel )

import Control.Monad.State ( StateT, evalStateT )
import Control.Monad.Writer ( Writer, runWriter )
import Control.Monad.Trans ( lift )

import Language.Sunroof ( JSA, JSFunction, JSNumber )
import Language.Sunroof.JS.Canvas ( JSCanvas )
import qualified Language.Sunroof.JS.Canvas as C

import Graphics.Rendering.Chart.Backend as G
import Graphics.Rendering.Chart.Backend.Impl
import Graphics.Rendering.Chart.Backend.Types

-- -----------------------------------------------------------------------
-- Rendering Backend Environment
-- -----------------------------------------------------------------------

-- | The environment we need to track when rendering to cairo
data CanvasEnv = CanvasEnv
  { ceAlignmentFns :: AlignmentFns
  , ceFontColor :: AlphaColour Double
  , ceFontSize :: Double
  }

-- | Produce a environment with no transformation and clipping. 
--   It will use the default styles.
defaultEnv :: AlignmentFns
           -> CanvasEnv
defaultEnv alignFns = CanvasEnv 
  { ceAlignmentFns = alignFns
  , ceFontColor = opaque black
  , ceFontSize = 12
  }

type RenderM a = StateT (JSCanvas, JSNumber, JSNumber, CanvasEnv) (Writer (JSA ())) a

runRenderM :: (JSCanvas, JSNumber, JSNumber, CanvasEnv) -> RenderM a -> (a, JSA ())
runRenderM env m = runWriter $ evalStateT m env

runJS :: (JSCanvas -> JSA ()) -> RenderM ()
runJS f = do 
  c <- cCanvas
  lift $ tell $ f c

runJS' :: JSA () -> RenderM ()
runJS' = lift . tell

cWidth :: RenderM JSNumber
cWidth = do
  (_c, w, _h, _env) <- get
  return w

cHeight :: RenderM JSNumber
cHeight = do
  (_c, _w, h, _env) <- get
  return h

cEnv :: RenderM CanvasEnv
cEnv = do
  (_c, _w, _h, env) <- get
  return env

modifyEnv :: (CanvasEnv -> CanvasEnv) -> RenderM ()
modifyEnv f = do
  (c, w, h, env) <- get
  put (c, w, h, f env)

cCanvas :: RenderM JSCanvas
cCanvas = do
  (c, _w, _h, _env) <- get
  return c

-- | Run this backends renderer.
runBackend :: CanvasEnv -- ^ Environment to start rendering with.
           -> ChartBackend a  -- ^ Chart render code.
           -> JSA (JSFunction (JSCanvas, JSNumber, JSNumber) ()
              -- ^ JSA code that provides a rendering function that will 
              --   take the canvas its width and height as parameters and renders
              --   the chart.
runBackend env m = function $ \(c, w, h) -> do
  fst $ runRenderM (c, w, h, env) 
      $ runBackend' (withDefaultStyle m)

runBackend' :: ChartBackend a -> RenderM a
runBackend' m = eval (view m)
  where
    eval :: ProgramView ChartBackendInstr a -> RenderM a
    eval (Return v)= return v
    eval (StrokePath p :>>= f) = cStrokePath p >>= step f
    eval (FillPath p :>>= f) = cFillPath p >>= step f
    eval (GetTextSize s :>>= f) = cTextSize s >>= step f
    eval (DrawText p s :>>= f) = cDrawText p s >>= step f
    eval (GetAlignments :>>= f) = fmap ceAlignmentFns cEnv >>= step f
    eval (WithTransform m p :>>= f) = cWithTransform m p >>= step f
    eval (WithFontStyle font p :>>= f) = cWithFontStyle font p >>= step f
    eval (WithFillStyle fs p :>>= f) = cWithFillStyle fs p >>= step f
    eval (WithLineStyle ls p :>>= f) = cWithLineStyle ls p >>= step f
    eval (WithClipRegion r p :>>= f) = cWithClipRegion r p >>= step f

    step :: (v -> ChartBackend a) -> v -> RenderM a
    step f =  runBackend' . f

walkPath :: Path -> RenderM ()
walkPath (MoveTo p path) = do
  runJS $ C.moveTo (toJsPoint p)
  walkPath path
walkPath (LineTo p path) = do
  runJS $ C.lineTo (toJsPoint p)
  walkPath path
walkPath (Arc p r a1 a2 path) = do
  runJS $ C.arc (toJsPoint p) (js r) (js a1, js a2) 
  walkPath path
walkPath (ArcNeg p r a1 a2 path) = do
  runJS $ C.arc' (toJsPoint p) (js r) (js a1, js a2) true
  walkPath path
walkPath End = return ()
walkPath Close = runJS $ C.closePath

cLocalState :: RenderM a -> RenderM a
cLocalState m = do
  runJS $ C.save
  v <- m
  runJS $ C.restore
  return v

cStrokePath :: Path -> RenderM ()
cStrokePath p = cLocalState $ do
  runJS $ C.beginPath 
  walkPath p 
  runJS $ C.stroke

cFillPath :: Path -> RenderM ()
cFillPath p = cLocalState $ do
  runJS $ C.beginPath
  walkPath p
  runJS $ C.fill

-- TODO
cTextSize :: String -> RenderM TextSize
cTextSize text = do
  -- Is this even possible right now?
  return $ TextSize 
    { textSizeWidth    = undefined
    , textSizeAscent   = undefined
    , textSizeDescent  = undefined
    , textSizeYBearing = undefined
    , textSizeHeight   = undefined
    }

cDrawText :: Point -> String -> RenderM ()
cDrawText p text = cLocalState $ do
  env <- cEnv
  runJS $ C.setFillStyle $ toJsColor $ ceFontColor env
  runJS $ C.fillText text (toJsPoint p)

cWithTransform :: Matrix -> ChartBackend a -> RenderM a
cWithTransform (G.Matrix a1 a2 b1 b2 c1 c2) p = cLocalState $ do
  runJS $ C.setTransform (js a1) (js a2) (js b1) (js b2) (js c1) (js c2)
  runBackend' p

-- TODO
cWithFontStyle :: FontStyle -> ChartBackend a -> RenderM a
cWithFontStyle font p = cLocalState $ do
  {- TODO
  C.selectFontFace (G._font_name font) 
                   (convertFontSlant $ G._font_slant font) 
                   (convertFontWeight $ G._font_weight font)
                                      -}
  runJS $ C.setFont $ convertFontStyle font
  modifyEnv $ \e -> e { ceFontSize = G._font_size font
                      , ceFontColor = G._font_color font }
  runBackend' p

cWithFillStyle :: FillStyle -> ChartBackend a -> RenderM a
cWithFillStyle fs p = cLocalState $ do
  runJS $ C.setFillStyle $ toJsColor $ G._fill_colour fs
  runBackend' p

cWithLineStyle :: LineStyle -> ChartBackend a -> RenderM a
cWithLineStyle ls p = cLocalState $ do
  runJS $ C.setStrokeStyle $ toJsColor $ G._line_color ls
  runJS $ C.setLineWidth $ js $ G._line_width ls
  runJS $ C.setLineCap $ convertLineCap $ G._line_cap ls
  runJS $ C.setLineJoin $ convertLineJoin $ G._line_join ls
  c <- cCanvas
  runJS' $ do
    dashes <- array $ G._line_dashes ls
    setLineDash dashes c
  runBackend' p

cWithClipRegion :: Rect -> ChartBackend a -> RenderM a
cWithClipRegion r p = cLocalState $ do
  setClipRegion r
  runBackend' p

-- TODO: Insert safty net into main trunk:
-- if (!c.setLineDash) {
--    c.setLineDash = function () {}
-- }
setLineDash :: JSArray JSNumber -> JSCanvas -> JSA ()
setLineDash dashes = invoke "setLineDash" dashes

-- -----------------------------------------------------------------------
-- Type Conversions: Chart -> Canvas
-- -----------------------------------------------------------------------

toJsColor :: AlphaColour Double -> JSString
toJsColor c = let (RGB r g b) = toSRGB $ colourChannel c
              in js $ concat [ "rgba("
                             , showInt r, ","
                             , showInt g, ","
                             , showInt b, ","
                             , show (alphaChannel c), ")"]
  where showInt :: Int -> String
        showInt d = toInteger $ round $ d * 255

toJsPoint :: Point -> (JSNumber, JSNumber)
toJsPoint p = (js (p_x p), js (p_y p))

setClipRegion :: Rect -> RenderM ()
setClipRegion (Rect p2 p3) = do
    runJS $ C.beginPath
    runJS $ C.moveTo (js (p_x p2), js (p_y p2))
    runJS $ C.lineTo (js (p_x p2), js (p_y p3))
    runJS $ C.lineTo (js (p_x p3), js (p_y p3))
    runJS $ C.lineTo (js (p_x p3), js (p_y p2))
    runJS $ C.lineTo (js (p_x p2), js (p_y p2))
    runJS $ C.clip

-- | Convert a charts line join to a cairo line join.
convertLineJoin :: G.LineJoin -> JSString
convertLineJoin lj = js $ case lj of
  G.LineJoinMiter -> "miter"
  G.LineJoinRound -> "round"
  G.LineJoinBevel -> "bevel"

-- | Convert a charts line cap to a cairo line cap.
convertLineCap :: G.LineCap -> JSString
convertLineCap lc = js $ case lc of
  G.LineCapRound  -> "round"
  G.LineCapButt   -> "butt"
  G.LineCapSquare -> "square"

convertFontStyle :: G.FontStyle -> JSString
convertFontStyle fs = undefined

convertFontSlant :: G.FontSlant -> String
convertFontSlant fs = case fs of
  G.FontSlantItalic  -> "italic"
  G.FontSlantNormal  -> ""
  G.FontSlantOblique -> "oblique"

convertFontWeight :: G.FontWeight -> String
convertFontWeight fw = case fw of
  G.FontWeightBold   -> "bold"
  G.FontWeightNormal -> ""
