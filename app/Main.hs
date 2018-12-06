{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  ) where

--------------------------------------------------------------------------------
import Control.Concurrent.STM
  ( TQueue
  , atomically
  , newTQueueIO
  , tryReadTQueue
  , writeTQueue
  )
import Control.Monad (liftM2, void, forM_)
import Control.Monad.RWS.Strict
  ( RWST
  , ask
  , asks
  , evalRWST
  , get
  , liftIO
  , modify
  , put
  )

import Control.Applicative
import Control.Category
import Control.Conditional
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Prelude hiding ((.), id)
import System.Random
import Text.PrettyPrint hiding ((<>))

import qualified Graphics.Rendering.FTGL as FTGL
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

data PieceKind
  = King
  | Queen
  | Rook
  | Knight
  | Bishop
  | Pawn
  deriving (Eq, Ord, Bounded, Enum)

data PieceColor
  = White
  | Black
  deriving (Eq, Ord, Bounded, Enum)

data Piece = Piece
  { pieceColor :: !PieceColor
  , pieceKind :: !PieceKind
  } deriving (Eq, Ord)

pieceChar :: Piece -> Char
pieceChar (Piece White King) = '♔'
pieceChar (Piece White Queen) = '♕'
pieceChar (Piece White Rook) = '♖'
pieceChar (Piece White Knight) = '♘'
pieceChar (Piece White Bishop) = '♗'
pieceChar (Piece White Pawn) = '♙'
pieceChar (Piece Black King) = '♚'
pieceChar (Piece Black Queen) = '♛'
pieceChar (Piece Black Rook) = '♜'
pieceChar (Piece Black Knight) = '♞'
pieceChar (Piece Black Bishop) = '♝'
pieceChar (Piece Black Pawn) = '♟'

allPieces :: [Piece]
allPieces = [Piece color kind | color <- [minBound ..], kind <- [minBound ..]]

type GPieceMap = M.Map Piece (GL.DisplayList, [Float])

data Position =
  Position !Int
           !Int
  deriving (Eq, Ord)

instance Show Position where
  show (Position i j) = "(" ++ (show i) ++ "," ++ (show j) ++ ")"

data SquareState
  = StillPiece Piece
  | MovingPiece { movingPiece :: Piece
                , movingTo :: Position
                , movingStartTime :: Double }

type SquareMap = M.Map Position SquareState

--------------------------------------------------------------------------------
data Env = Env
  { envEventsChan :: TQueue Event
  , envWindow :: !GLFW.Window
  , envZDistClosest :: !Double
  , envZDistFarthest :: !Double
  }

data State = State
  { stateWindowWidth :: !Int
  , stateWindowHeight :: !Int
  , stateXAngle :: !Double
  , stateYAngle :: !Double
  , stateZAngle :: !Double
  , stateZDist :: !Double
  , stateMouseDown :: !Bool
  , stateDragging :: !Bool
  , stateDragStartX :: !Double
  , stateDragStartY :: !Double
  , stateDragStartXAngle :: !Double
  , stateDragStartYAngle :: !Double
  , stateSquares :: !SquareMap
  , stateGPieceFonts :: ![FTGL.Font]
  , stateGPieces :: !GPieceMap
  , stateGBoard :: !GL.DisplayList
  , stateGBoardSize :: !Float
  }

type Demo = RWST Env () State IO

--------------------------------------------------------------------------------
data Event
  = EventError !GLFW.Error
               !String
  | EventWindowPos !GLFW.Window
                   !Int
                   !Int
  | EventWindowSize !GLFW.Window
                    !Int
                    !Int
  | EventWindowClose !GLFW.Window
  | EventWindowRefresh !GLFW.Window
  | EventWindowFocus !GLFW.Window
                     !Bool
  | EventWindowIconify !GLFW.Window
                       !Bool
  | EventFramebufferSize !GLFW.Window
                         !Int
                         !Int
  | EventMouseButton !GLFW.Window
                     !GLFW.MouseButton
                     !GLFW.MouseButtonState
                     !GLFW.ModifierKeys
  | EventCursorPos !GLFW.Window
                   !Double
                   !Double
  | EventCursorEnter !GLFW.Window
                     !GLFW.CursorState
  | EventScroll !GLFW.Window
                !Double
                !Double
  | EventKey !GLFW.Window
             !GLFW.Key
             !Int
             !GLFW.KeyState
             !GLFW.ModifierKeys
  | EventChar !GLFW.Window
              !Char
  deriving (Show)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  let width = 1600
      height = 900
  eventsChan <- newTQueueIO :: IO (TQueue Event)
  withWindow width height "GLFW-b-demo" $ \win -> do
    GLFW.setErrorCallback $ Just $ errorCallback eventsChan
    GLFW.setWindowPosCallback win $ Just $ windowPosCallback eventsChan
    GLFW.setWindowSizeCallback win $ Just $ windowSizeCallback eventsChan
    GLFW.setWindowCloseCallback win $ Just $ windowCloseCallback eventsChan
    GLFW.setWindowRefreshCallback win $ Just $ windowRefreshCallback eventsChan
    GLFW.setWindowFocusCallback win $ Just $ windowFocusCallback eventsChan
    GLFW.setWindowIconifyCallback win $ Just $ windowIconifyCallback eventsChan
    GLFW.setFramebufferSizeCallback win $
      Just $ framebufferSizeCallback eventsChan
    GLFW.setMouseButtonCallback win $ Just $ mouseButtonCallback eventsChan
    GLFW.setCursorPosCallback win $ Just $ cursorPosCallback eventsChan
    GLFW.setCursorEnterCallback win $ Just $ cursorEnterCallback eventsChan
    GLFW.setScrollCallback win $ Just $ scrollCallback eventsChan
    GLFW.setKeyCallback win $ Just $ keyCallback eventsChan
    GLFW.setCharCallback win $ Just $ charCallback eventsChan
    GLFW.swapInterval 1
    GL.position (GL.Light 0) GL.$= GL.Vertex4 0 0 800 0
    GL.light (GL.Light 0) GL.$= GL.Enabled
    GL.lighting GL.$= GL.Enabled
    GL.cullFace GL.$= Nothing
    GL.depthFunc GL.$= Just GL.Less
    GL.clearColor GL.$= GL.Color4 0.05 0.2 0.05 1
    GL.normalize GL.$= GL.Enabled
    (fbWidth, fbHeight) <- GLFW.getFramebufferSize win
    let gBoardSize = 800
    gBoard <-
      makeBoard gBoardSize (GL.Color4 0.8 0.7 0.7 1) (GL.Color4 0.7 0.6 0.6 1)
    let squareList =
          map (fmap StillPiece) $
          concat
            [ [ (Position 0 j0, Piece col Rook)
              , (Position 1 j0, Piece col Knight)
              , (Position 2 j0, Piece col Bishop)
              , (Position 3 j0, Piece col Queen)
              , (Position 4 j0, Piece col King)
              , (Position 5 j0, Piece col Bishop)
              , (Position 6 j0, Piece col Knight)
              , (Position 7 j0, Piece col Rook)
            ] ++
            [(Position i j1, Piece col Pawn) | i <- [0 .. 7]]
            | (j0, j1, col) <- [(0, 1, White), (7, 6, Black)]
            ]
    gPieceFonts <-
      sequence $
      [ "/Users/puvar/chessfonts/Symbola.ttf"
      , "/Users/puvar/chessfonts/CaslonRoman.ttf"
      , "/Users/puvar/chessfonts/Code200365k.ttf"
      , "/Users/puvar/chessfonts/FreeSerif.ttf"
      , "/Users/puvar/chessfonts/Quivira.ttf"
      , "/Users/puvar/chessfonts/chrysuni.ttf"
      , "/Users/puvar/chessfonts/DejaVuSans.ttf"
      ] <**>
      [FTGL.createPolygonFont, FTGL.createBufferFont, FTGL.createExtrudeFont]
    sequence_ [FTGL.setFontDepth font 20 | font <- gPieceFonts]
    gPieces <- makeGPieces (head gPieceFonts)
    let zDistClosest = 1000
        zDistFarthest = zDistClosest + 1200
        zDist = zDistClosest + ((zDistFarthest - zDistClosest) / 2)
        env =
          Env
            { envEventsChan = eventsChan
            , envWindow = win
            , envZDistClosest = zDistClosest
            , envZDistFarthest = zDistFarthest
            }
        state =
          State
            { stateWindowWidth = fbWidth
            , stateWindowHeight = fbHeight
            , stateXAngle = -10
            , stateYAngle = 0
            , stateZAngle = 0
            , stateZDist = zDist
            , stateMouseDown = False
            , stateDragging = False
            , stateDragStartX = 0
            , stateDragStartY = 0
            , stateDragStartXAngle = 0
            , stateDragStartYAngle = 0
            , stateSquares = M.fromList squareList
            , stateGPieceFonts = gPieceFonts
            , stateGPieces = gPieces
            , stateGBoard = gBoard
            , stateGBoardSize = gBoardSize
            }
    runDemo env state
  putStrLn "ended!"

--    GL.spotExponent (GL.Light 0) GL.$= 64
--    GL.ambient (GL.Light 0) GL.$= GL.Color4 0.5 0.5 0.5 1
--    GL.diffuse (GL.Light 0) GL.$= GL.Color4 0.3 0.3 0.3 0
--    GL.specular (GL.Light 0) GL.$= GL.Color4 0.9 0.9 0.9 1
-------------------------------------------------------------------------------
-- Make GL elements.
-------------------------------------------------------------------------------
makeGPieces :: FTGL.Font -> IO GPieceMap
makeGPieces font =
  M.fromList <$>
  sequence
    [ do let effPiece = Piece Black kind
             gColor =
               if color == White
                 then (GL.Color4 0.6 0.6 0.3 1)
                 else (GL.Color4 0.2 0.1 0.1 1)
         txt <- makeText font [pieceChar effPiece] 72 72 gColor
         return (piece, txt)
    | piece@(Piece color kind) <- allPieces
    ]

makeText ::
     FTGL.Font
  -> String
  -> Int
  -> Int
  -> GL.Color4 GL.GLfloat
  -> IO (GL.DisplayList, [Float])
makeText font txt sz1 sz2 color = do
  void $ FTGL.setFontFaceSize font sz1 sz2
  dl <-
    GL.defineNewList GL.Compile $ do
      normal 0 0 1
      GL.colorMaterial GL.$= Just (GL.FrontAndBack, GL.AmbientAndDiffuse)
      GL.shadeModel GL.$= GL.Smooth
      GL.color color
      FTGL.renderFont font txt FTGL.All
  bbox <- FTGL.getFontBBox font txt
  return (dl, bbox)

--  layout <- FTGL.createSimpleLayout
--  FTGL.setLayoutFont layout font
--  FTGL.setLayoutLineLength layout 10
--------------------------------------------------------------------------------
makeBoard ::
     Float -> GL.Color4 GL.GLfloat -> GL.Color4 GL.GLfloat -> IO GL.DisplayList
makeBoard size color1 color2 =
  GL.defineNewList GL.Compile $ do
    let start = -(size / 2)
        delta = size / 8
        small = 0
        z = -20
    GL.colorMaterial GL.$= Just (GL.FrontAndBack, GL.Ambient)
    GL.shadeModel GL.$= GL.Smooth
    normal 0 0 1
    GL.renderPrimitive GL.Quads $
      forM_ [0 .. 63] $ \i -> do
        let (x, y) = (divMod i 8) :: (Int, Int)
        GL.shadeModel GL.$= GL.Smooth
        if even (x + y)
          then GL.color color2
          else GL.color color1
        let rx = fromIntegral x
            ry = fromIntegral y
        vertex (start + rx * delta + small) (start + ry * delta + small) z
        vertex (start + (rx + 1) * delta - small) (start + ry * delta + small) z
        vertex
          (start + (rx + 1) * delta - small)
          (start + (ry + 1) * delta - small)
          z
        vertex (start + rx * delta + small) (start + (ry + 1) * delta - small) z
  where


vertex :: Float -> Float -> Float -> IO ()
vertex x y z =
  GL.vertex $
  ((GL.Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)) :: GL.Vertex3 GL.GLfloat)

normal :: Float -> Float -> Float -> IO ()
normal x y z =
  GL.normal $
  ((GL.Normal3 (realToFrac x) (realToFrac y) (realToFrac z)) :: GL.Normal3 GL.GLfloat)

--------------------------------------------------------------------------------
-- GLFW-b is made to be very close to the C API, so creating a window is pretty
-- clunky by Haskell standards. A higher-level API would have some function
-- like withWindow.
withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  when r $ do
    GLFW.windowHint $ GLFW.WindowHint'Visible False
    m <- GLFW.createWindow width height title Nothing Nothing
    GLFW.pollEvents
    case m of
      (Just win) -> do
        GLFW.makeContextCurrent m
        GLFW.showWindow win
        f win
        GLFW.setErrorCallback $ Just simpleErrorCallback
        GLFW.destroyWindow win
      Nothing -> return ()
    GLFW.terminate
  where
    simpleErrorCallback e s = putStrLn $ unwords [show e, show s]

--------------------------------------------------------------------------------
-- Each callback does just one thing: write an appropriate Event to the events
-- TQueue.
errorCallback :: TQueue Event -> GLFW.Error -> String -> IO ()
windowPosCallback :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
windowSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
windowCloseCallback :: TQueue Event -> GLFW.Window -> IO ()
windowRefreshCallback :: TQueue Event -> GLFW.Window -> IO ()
windowFocusCallback :: TQueue Event -> GLFW.Window -> Bool -> IO ()
windowIconifyCallback :: TQueue Event -> GLFW.Window -> Bool -> IO ()
framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
mouseButtonCallback ::
     TQueue Event
  -> GLFW.Window
  -> GLFW.MouseButton
  -> GLFW.MouseButtonState
  -> GLFW.ModifierKeys
  -> IO ()
cursorPosCallback :: TQueue Event -> GLFW.Window -> Double -> Double -> IO ()
cursorEnterCallback :: TQueue Event -> GLFW.Window -> GLFW.CursorState -> IO ()
scrollCallback :: TQueue Event -> GLFW.Window -> Double -> Double -> IO ()
keyCallback ::
     TQueue Event
  -> GLFW.Window
  -> GLFW.Key
  -> Int
  -> GLFW.KeyState
  -> GLFW.ModifierKeys
  -> IO ()
charCallback :: TQueue Event -> GLFW.Window -> Char -> IO ()
errorCallback tc e s = atomically $ writeTQueue tc $ EventError e s

windowPosCallback tc win x y =
  atomically $ writeTQueue tc $ EventWindowPos win x y

windowSizeCallback tc win w h =
  atomically $ writeTQueue tc $ EventWindowSize win w h

windowCloseCallback tc win = atomically $ writeTQueue tc $ EventWindowClose win

windowRefreshCallback tc win =
  atomically $ writeTQueue tc $ EventWindowRefresh win

windowFocusCallback tc win fa =
  atomically $ writeTQueue tc $ EventWindowFocus win fa

windowIconifyCallback tc win ia =
  atomically $ writeTQueue tc $ EventWindowIconify win ia

framebufferSizeCallback tc win w h =
  atomically $ writeTQueue tc $ EventFramebufferSize win w h

mouseButtonCallback tc win mb mba mk =
  atomically $ writeTQueue tc $ EventMouseButton win mb mba mk

cursorPosCallback tc win x y =
  atomically $ writeTQueue tc $ EventCursorPos win x y

cursorEnterCallback tc win ca =
  atomically $ writeTQueue tc $ EventCursorEnter win ca

scrollCallback tc win x y = atomically $ writeTQueue tc $ EventScroll win x y

keyCallback tc win k sc ka mk =
  atomically $ writeTQueue tc $ EventKey win k sc ka mk

charCallback tc win c = atomically $ writeTQueue tc $ EventChar win c

--------------------------------------------------------------------------------
runDemo :: Env -> State -> IO ()
runDemo env state = do
  printInstructions
  void $ evalRWST (adjustWindow >> run) env state

run :: Demo ()
run = do
  win <- asks envWindow
  draw
  liftIO $ do
    GLFW.swapBuffers win
    GL.flush -- not necessary, but someone recommended it
    GLFW.pollEvents
  processEvents
  state <- get
  if stateDragging state
    then do
      let sodx = stateDragStartX state
          sody = stateDragStartY state
          sodxa = stateDragStartXAngle state
          sodya = stateDragStartYAngle state
      (x, y) <- liftIO $ GLFW.getCursorPos win
      let myrot = (x - sodx) / 2
          mxrot = (y - sody) / 2
      modify $ \s ->
        s {stateXAngle = sodxa + mxrot, stateYAngle = sodya + myrot}
    else do
      (kxrot, kyrot) <- liftIO $ getCursorKeyDirections win
      (jxrot, jyrot) <- liftIO $ getJoystickDirections GLFW.Joystick'1
      modify $ \s ->
        s
          { stateXAngle = stateXAngle s + (2 * kxrot) + (2 * jxrot)
          , stateYAngle = stateYAngle s + (2 * kyrot) + (2 * jyrot)
          }
  q <- liftIO $ GLFW.windowShouldClose win
  unless q run

--  mt <- liftIO GLFW.getTime
--  modify $ \s -> s {stateGearZAngle = maybe 0 (realToFrac . (100 *)) mt}
processEvents :: Demo ()
processEvents = do
  tc <- asks envEventsChan
  me <- liftIO $ atomically $ tryReadTQueue tc
  case me of
    Just e -> do
      processEvent e
      processEvents
    Nothing -> return ()

processEvent :: Event -> Demo ()
processEvent ev =
  case ev of
    (EventError e s) -> do
      printEvent "error" [show e, show s]
      win <- asks envWindow
      liftIO $ GLFW.setWindowShouldClose win True
    (EventWindowPos _ x y) -> printEvent "window pos" [show x, show y]
    (EventWindowSize _ width height) ->
      printEvent "window size" [show width, show height]
    (EventWindowClose _) -> printEvent "window close" []
    (EventWindowRefresh _) -> printEvent "window refresh" []
    (EventWindowFocus _ fs) -> printEvent "window focus" [show fs]
    (EventWindowIconify _ is) -> printEvent "window iconify" [show is]
    (EventFramebufferSize _ width height) -> do
      printEvent "framebuffer size" [show width, show height]
      modify $ \s -> s {stateWindowWidth = width, stateWindowHeight = height}
      adjustWindow
    (EventMouseButton _ mb mbs mk) -> do
      printEvent "mouse button" [show mb, show mbs, showModifierKeys mk]
      when (mb == GLFW.MouseButton'1) $ do
        let pressed = mbs == GLFW.MouseButtonState'Pressed
        modify $ \s -> s {stateMouseDown = pressed}
        unless pressed $ modify $ \s -> s {stateDragging = False}
    (EventCursorPos _ x y) -> do
      let x' = round x :: Int
          y' = round y :: Int
      printEvent "cursor pos" [show x', show y']
      state <- get
      when (stateMouseDown state && not (stateDragging state)) $
        put $
        state
          { stateDragging = True
          , stateDragStartX = x
          , stateDragStartY = y
          , stateDragStartXAngle = stateXAngle state
          , stateDragStartYAngle = stateYAngle state
          }
    (EventCursorEnter _ cs) -> printEvent "cursor enter" [show cs]
    (EventScroll _ x y) -> do
      let x' = round x :: Int
          y' = round y :: Int
      printEvent "scroll" [show x', show y']
      env <- ask
      modify $ \s ->
        s
          { stateZDist =
              let zDist' = stateZDist s + realToFrac (negate $ y / 2 * 100)
               in curb (envZDistClosest env) (envZDistFarthest env) zDist'
          }
      adjustWindow
    (EventKey win k scancode ks mk) -> do
      printEvent "key" [show k, show scancode, show ks, showModifierKeys mk]
      when (ks == GLFW.KeyState'Pressed) $
              -- Q, Esc: exit
       do
        when (k == GLFW.Key'Q || k == GLFW.Key'Escape) $
          liftIO $ GLFW.setWindowShouldClose win True
              -- ?: print instructions
        when (k == GLFW.Key'F) $ do
          state <- get
          let fonts = stateGPieceFonts state
              fonts' =
                if GLFW.modifierKeysShift mk
                  then last fonts : init fonts
                  else (tail fonts) ++ [head fonts]
          m <- liftIO $ makeGPieces (head fonts')
          modify $ \s -> s {stateGPieceFonts = fonts', stateGPieces = m}
        when (k == GLFW.Key'M) $ do
          state <- get
          let squares = stateSquares state
          let sqList =
                M.toList $
                M.filter
                  (\case
                     (StillPiece _) -> True
                     _ -> False) $
                squares
          let searchEmpty = do
                (i :: Int) <- randomRIO (0, 7)
                (j :: Int) <- randomRIO (0, 7)
                if M.member (Position i j) squares
                  then searchEmpty
                  else return (i, j)
          (randI, randJ) <- liftIO searchEmpty
          (randSq :: Int) <- liftIO $ randomRIO (0, length sqList - 1)
          currentTime <- liftIO $ maybe 0 id <$> GLFW.getTime
          let (posFrom, StillPiece piece) = sqList !! randSq
          modify $ \s ->
            s
              { stateSquares =
                  M.insert
                    posFrom
                    MovingPiece
                      { movingPiece = piece
                      , movingTo = Position randI randJ
                      , movingStartTime = currentTime
                      }
                    (stateSquares state)
              }
        when (k == GLFW.Key'Slash && GLFW.modifierKeysShift mk) $
          liftIO printInstructions
              -- i: print GLFW information
        when (k == GLFW.Key'I) $ liftIO $ printInformation win
    (EventChar _ c) -> printEvent "char" [show c]

adjustWindow :: Demo ()
adjustWindow = do
  state <- get
  let width = stateWindowWidth state
      height = stateWindowHeight state
      zDist = stateZDist state
  let pos = GL.Position 0 0
      size = GL.Size (fromIntegral width) (fromIntegral height)
      h = fromIntegral height / fromIntegral width :: Double
      znear = 200 :: Double
      zfar = 4000 :: Double
      xmax = znear * 0.5 :: Double
  liftIO $ do
    GL.viewport GL.$= (pos, size)
    GL.matrixMode GL.$= GL.Projection
    GL.loadIdentity
    GL.frustum
      (realToFrac $ -xmax)
      (realToFrac xmax)
      (realToFrac $ -xmax * realToFrac h)
      (realToFrac $ xmax * realToFrac h)
      (realToFrac znear)
      (realToFrac zfar)
    GL.matrixMode GL.$= GL.Modelview 0
    GL.loadIdentity
    GL.translate
      (GL.Vector3 0 0 (negate $ realToFrac zDist) :: GL.Vector3 GL.GLfloat)
    putStrLn ("zDist " ++ (show zDist))

draw :: Demo ()
draw = do
  state <- get
  let gBoard = stateGBoard state
      squares = stateSquares state
      xa = stateXAngle state
      ya = stateYAngle state
      za = stateZAngle state
  stateMods <-
    liftIO $ do
      currentTime <- maybe 0 id <$> GLFW.getTime
      GL.clear [GL.ColorBuffer, GL.DepthBuffer]
      GL.preservingMatrix $ do
        GL.translate vec1
        GL.rotate (realToFrac xa) xunit
        GL.rotate (realToFrac ya) yunit
        GL.rotate (realToFrac za) zunit
        GL.preservingMatrix $ GL.callList gBoard
        catMaybes <$>
          sequence
            [ putPiece currentTime squareState position state
            | (position, squareState) <- M.toList squares
            ]
  sequence_ [modify stateMod | stateMod <- stateMods]
  where
    vec1 = GL.Vector3 0 0 0 :: GL.Vector3 GL.GLfloat

xunit :: GL.Vector3 GL.GLfloat
xunit = GL.Vector3 1 0 0

yunit :: GL.Vector3 GL.GLfloat
yunit = GL.Vector3 0 1 0

zunit :: GL.Vector3 GL.GLfloat
zunit = GL.Vector3 0 0 1

squareCenter :: Fractional a => Position -> State -> (a, a, a)
squareCenter (Position i j) = do
  gBoardSize <- realToFrac <$> stateGBoardSize
  let start = -gBoardSize / 2
      delta = gBoardSize / 8
  return
    ( start + ((fromIntegral i) + 0.5) * delta
    , start + ((fromIntegral j) + 0.5) * delta
    , 0)

drawPiece ::
     Piece
  -> (Float, Float, Float)
  -> (Float, Float, Float)
  -> State
  -> IO (Maybe (State -> State))
drawPiece piece (xa, ya, za) (x, y, z) = do
  pieces <- stateGPieces
  return $
    case M.lookup piece pieces of
      Just (gTxt, [tx1, ty1, tz1, tx2, ty2, tz2]) -> do
        let shiftx = x - (tx2 + tx1) / 2
            shifty = y - (ty2 + ty1) / 2
            shiftz = z - (tz2 + tz1) / 2
        GL.preservingMatrix $ do
          GL.translate $
            ((GL.Vector3 shiftx shifty shiftz) :: GL.Vector3 GL.GLfloat)
          GL.rotate (realToFrac xa) xunit
          GL.rotate (realToFrac ya) yunit
          GL.rotate (realToFrac za) zunit
          GL.callList gTxt
        return Nothing
      Just _ -> error "bad bbox for font"
      Nothing -> error "piece not found"

putPiece ::
     Double -> SquareState -> Position -> State -> IO (Maybe (State -> State))
putPiece _ (StillPiece piece) pos =
  drawPiece piece (0, 0, 0) =<< squareCenter pos
putPiece currentTime (MovingPiece { movingPiece = piece
                                  , movingTo = to
                                  , movingStartTime = startTime
                                  }) from = do
  ((fromX, fromY, _) :: (Double, Double, Double)) <- squareCenter from
  ((toX, toY, _) :: (Double, Double, Double)) <- squareCenter to
  gBoardSize <- stateGBoardSize
  let distX = toX - fromX
      distY = toY - fromY
      dist = sqrt $ distX * distX + distY * distY
      g = 4 * (realToFrac gBoardSize) * tg
      vel = sqrt $ g * dist / 2 / tg
      tg = if' (pieceKind piece == King) 4 4
      velX = vel * distX / dist
      velY = vel * distY / dist
  state <- id
  return $ do
    let t = realToFrac $ currentTime - startTime
        rotX = 2 * sin (2 * pi * vel / dist * t)
        rotY = 2 * sin (4 * pi * vel / dist * t)
        rotZ =
          if pieceKind piece == Knight
            then (if' (pieceColor piece == Black) 360 (-360)) * vel / dist * t
            else 0
        stop = t >= dist / vel
        (currentX, currentY, currentZ) =
          if stop
            then (toX, toY, 0.0)
            else ( fromX + velX * t
                 , fromY + velY * t
                 , vel * tg * t - 0.5 * g * t * t)
    when stop $ putStrLn ("stopped " ++ (show from))
    void $
      drawPiece
        piece
        (realToFrac rotX, realToFrac rotY, realToFrac rotZ)
        (realToFrac currentX, realToFrac currentY, realToFrac currentZ)
        state
    return $
      if stop
        then Just stopMoving
        else Nothing
  where
    stopMoving :: State -> State
    stopMoving state =
      state
        { stateSquares =
            M.insert to (StillPiece piece) $ M.delete from (stateSquares state)
        }

getCursorKeyDirections :: GLFW.Window -> IO (Double, Double)
getCursorKeyDirections win = do
  let k2i = fmap (bool 1 0 . isPress) . GLFW.getKey win
      (@-) x y = (liftM2 (-)) (k2i x) (k2i y)
  liftM2 (,) (GLFW.Key'Down @- GLFW.Key'Up) (GLFW.Key'Right @- GLFW.Key'Left)

getJoystickDirections :: GLFW.Joystick -> IO (Double, Double)
getJoystickDirections js = do
  maxes <- GLFW.getJoystickAxes js
  return $
    case maxes of
      (Just (x:y:_)) -> (-y, x)
      _ -> (0, 0)

isPress :: GLFW.KeyState -> Bool
isPress GLFW.KeyState'Pressed = True
isPress GLFW.KeyState'Repeating = True
isPress _ = False

--------------------------------------------------------------------------------
printInstructions :: IO ()
printInstructions =
  putStrLn $
  render $
  nest
    4
    (text "------------------------------------------------------------" $+$
     text "'?': Print these instructions" $+$
     text "'i': Print GLFW information" $+$
     text "" $+$
     text "* Mouse cursor, keyboard cursor keys, and/or joystick" $+$
     text "  control rotation." $+$
     text "* Mouse scroll wheel controls distance from scene." $+$
     text "------------------------------------------------------------")

printInformation :: GLFW.Window -> IO ()
printInformation win = do
  version <- GLFW.getVersion
  versionString <- GLFW.getVersionString
  monitorInfos <- runMaybeT getMonitorInfos
  joystickNames <- getJoystickNames
  clientAPI <- GLFW.getWindowClientAPI win
  cv0 <- GLFW.getWindowContextVersionMajor win
  cv1 <- GLFW.getWindowContextVersionMinor win
  cv2 <- GLFW.getWindowContextVersionRevision win
  robustness <- GLFW.getWindowContextRobustness win
  forwardCompat <- GLFW.getWindowOpenGLForwardCompat win
  debug <- GLFW.getWindowOpenGLDebugContext win
  profile <- GLFW.getWindowOpenGLProfile win
  putStrLn $
    render $
    nest
      4
      (text "------------------------------------------------------------" $+$
       text "GLFW C library:" $+$
       nest
         4
         (text "Version:" <+>
          renderVersion version $+$ text "Version string:" <+>
          renderVersionString versionString) $+$
       text "Monitors:" $+$
       nest 4 (renderMonitorInfos monitorInfos) $+$
       text "Joysticks:" $+$
       nest 4 (renderJoystickNames joystickNames) $+$
       text "OpenGL context:" $+$
       nest
         4
         (text "Client API:" <+>
          renderClientAPI clientAPI $+$ text "Version:" <+>
          renderContextVersion cv0 cv1 cv2 $+$ text "Robustness:" <+>
          renderContextRobustness robustness $+$ text "Forward compatibility:" <+>
          renderForwardCompat forwardCompat $+$ text "Debug:" <+>
          renderDebug debug $+$ text "Profile:" <+> renderProfile profile) $+$
       text "------------------------------------------------------------")
  where
    renderVersion (GLFW.Version v0 v1 v2) =
      text $ intercalate "." $ map show [v0, v1, v2]
    renderVersionString = text . show
    renderMonitorInfos = maybe (text "(error)") (vcat . map renderMonitorInfo)
    renderMonitorInfo (name, (x, y), (w, h), vms) =
      text (show name) $+$
      nest 4 (location <+> size $+$ fsep (map renderVideoMode vms))
      where
        location = int x <> text "," <> int y
        size = int w <> text "x" <> int h <> text "mm"
    renderVideoMode (GLFW.VideoMode w h r g b rr) =
      brackets $ res <+> rgb <+> hz
      where
        res = int w <> text "x" <> int h
        rgb = int r <> text "x" <> int g <> text "x" <> int b
        hz = int rr <> text "Hz"
    renderJoystickNames pairs =
      vcat $ map (\(js, name) -> text (show js) <+> text (show name)) pairs
    renderContextVersion v0 v1 v2 =
      hcat [int v0, text ".", int v1, text ".", int v2]
    renderClientAPI = text . show
    renderContextRobustness = text . show
    renderForwardCompat = text . show
    renderDebug = text . show
    renderProfile = text . show

type MonitorInfo = (String, (Int, Int), (Int, Int), [GLFW.VideoMode])

getMonitorInfos :: MaybeT IO [MonitorInfo]
getMonitorInfos = getMonitors >>= mapM getMonitorInfo
  where
    getMonitors :: MaybeT IO [GLFW.Monitor]
    getMonitors = MaybeT GLFW.getMonitors
    getMonitorInfo :: GLFW.Monitor -> MaybeT IO MonitorInfo
    getMonitorInfo mon = do
      name <- getMonitorName mon
      vms <- getVideoModes mon
      MaybeT $ do
        pos <- liftIO $ GLFW.getMonitorPos mon
        size <- liftIO $ GLFW.getMonitorPhysicalSize mon
        return $ Just (name, pos, size, vms)
    getMonitorName :: GLFW.Monitor -> MaybeT IO String
    getMonitorName mon = MaybeT $ GLFW.getMonitorName mon
    getVideoModes :: GLFW.Monitor -> MaybeT IO [GLFW.VideoMode]
    getVideoModes mon = MaybeT $ GLFW.getVideoModes mon

getJoystickNames :: IO [(GLFW.Joystick, String)]
getJoystickNames = catMaybes `fmap` mapM getJoystick joysticks
  where
    getJoystick js =
      fmap (maybe Nothing (\name -> Just (js, name))) (GLFW.getJoystickName js)

--------------------------------------------------------------------------------
printEvent :: String -> [String] -> Demo ()
printEvent cbname fields = liftIO $ putStrLn $ cbname ++ ": " ++ unwords fields

showModifierKeys :: GLFW.ModifierKeys -> String
showModifierKeys mk = "[mod keys: " ++ keys ++ "]"
  where
    keys =
      if null xs
        then "none"
        else unwords xs
    xs = catMaybes ys
    ys =
      [ if GLFW.modifierKeysShift mk
          then Just "shift"
          else Nothing
      , if GLFW.modifierKeysControl mk
          then Just "control"
          else Nothing
      , if GLFW.modifierKeysAlt mk
          then Just "alt"
          else Nothing
      , if GLFW.modifierKeysSuper mk
          then Just "super"
          else Nothing
      ]

curb :: Ord a => a -> a -> a -> a
curb l h x
  | x < l = l
  | x > h = h
  | otherwise = x

--------------------------------------------------------------------------------
joysticks :: [GLFW.Joystick]
joysticks =
  [ GLFW.Joystick'1
  , GLFW.Joystick'2
  , GLFW.Joystick'3
  , GLFW.Joystick'4
  , GLFW.Joystick'5
  , GLFW.Joystick'6
  , GLFW.Joystick'7
  , GLFW.Joystick'8
  , GLFW.Joystick'9
  , GLFW.Joystick'10
  , GLFW.Joystick'11
  , GLFW.Joystick'12
  , GLFW.Joystick'13
  , GLFW.Joystick'14
  , GLFW.Joystick'15
  , GLFW.Joystick'16
  ]
