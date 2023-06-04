module UdGraphic (
  display,
  Llapis(..),
  Comanda(..),
  Distancia,
  Angle,
  blanc, negre, vermell, verd, verd_d, verd_D, blau, groc, groc_d, groc_D, marro, rosa, taronja, violeta, gris,
  execute
)
where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT hiding (Angle)
import Data.IORef
import Data.List
import Control.Monad( liftM, liftM2, liftM3 )
import System.Random
import Test.QuickCheck
infixr 5 :#:


-- Punts

data Pnt = Pnt Float Float
  deriving (Eq,Ord,Show)

instance Num Pnt where
  Pnt x y + Pnt x' y'  =  Pnt (x+x') (y+y')
  Pnt x y - Pnt x' y'  =  Pnt (x-x') (y-y')
  Pnt x y * Pnt x' y'  =  Pnt (x*x') (y*y')
  fromInteger          =  scalar . fromInteger
  abs (Pnt x y)        =  Pnt (abs x) (abs y)
  signum (Pnt x y)     =  Pnt (signum x) (signum y)

instance Fractional Pnt where
  Pnt x y / Pnt x' y'  =  Pnt (x/x') (y/y')
  fromRational         =  scalar . fromRational

scalar :: Float -> Pnt
scalar x  =  Pnt x x

scalarMin :: Pnt -> Pnt
scalarMin (Pnt x y)  =  scalar (x `min` y)

scalarMax :: Pnt -> Pnt
scalarMax (Pnt x y)  =  scalar (x `max` y)

dimensions :: Pnt -> (Int,Int)
dimensions (Pnt x y)  =  (ceiling x, ceiling y)

lub :: Pnt -> Pnt -> Pnt
Pnt x y `lub` Pnt x' y'  =  Pnt (x `max` x') (y `max` y')

glb :: Pnt -> Pnt -> Pnt
Pnt x y `glb` Pnt x' y'  =  Pnt (x `min` x') (y `min` y')

pointToSize :: Pnt -> Size
pointToSize (Pnt x y) = Size (ceiling x) (ceiling y)

sizeToPoint :: Size -> Pnt
sizeToPoint (Size x y) = Pnt (fromIntegral x) (fromIntegral y)

-- Colors

data Llapis = Color' GL.GLfloat GL.GLfloat GL.GLfloat
            | Transparent
            deriving (Eq, Ord, Show)

pencilToRGB :: Llapis -> GL.Color3 GL.GLfloat
pencilToRGB (Color' r g b)  =  GL.Color3 r g b
pencilToRGB Transparent  =  error "pencilToRGB: transparent"

blanc, negre, vermell, verd, blau :: Llapis
blanc   = Color' 1.0 1.0 1.0
negre   = Color' 0.0 0.0 0.0
gris    = Color' 0.5 0.5 0.5
vermell = Color' 1.0 0.0 0.0
verd    = Color' 0.0 1.0 0.0
verd_d  = Color' 0.0 0.5 0.0
verd_D  = Color' 0.031 0.361 0.224
blau    = Color' 0.0 0.0 1.0
groc    = Color' 1.0 1.0 0.0
groc_d  = Color' 0.89 0.686 0.161
groc_D  = Color' 0.561 0.447 0.157
marro   = Color' 0.6 0.3 0.0
taronja = Color' 1.0 0.5 0.0
rosa    = Color' 1.0 0.0 1.0
violeta = Color' 0.5 0.0 1.0


-- Lines

data Ln = Ln Llapis Pnt Pnt
  deriving (Eq,Ord,Show)


-- Window parameters

theCanvas :: Pnt
theCanvas  =  Pnt 800 800

theBGcolor :: GL.Color3 GL.GLfloat
theBGcolor = pencilToRGB blanc



-- Main drawing and window functions

display :: Comanda -> IO ()
display c = do
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize  $= pointToSize theCanvas
  getArgsAndInitialize
  w <- createWindow "pencilcil Graphics"
  displayCallback $= draw c
  reshapeCallback $= Just (\x -> (viewport $= (Position 0 0, x)))
  --actionOnWindowClose $= ContinueExectuion
  draw c
  mainLoop

draw :: Comanda -> IO ()
draw c = do clear [ColorBuffer]
            loadIdentity
            background
            toGraphic $ rescale $ execute c
            swapBuffers

toGraphic :: [Ln] -> IO ()
toGraphic lines  = sequence_ (map f lines)
  where
  f (Ln pencil startP endP)  =
    GL.color (pencilToRGB pencil) >>
    GL.renderPrimitive GL.LineStrip (toVertex startP >> toVertex endP)

background :: IO ()
background = do GL.color theBGcolor
                GL.renderPrimitive GL.Polygon $ mapM_ GL.vertex
                      [GL.Vertex3 (-1) (-1) 0,
                       GL.Vertex3   1  (-1) 0,
                       GL.Vertex3   1    1  0,
                       GL.Vertex3 (-1)   1 (0::GL.GLfloat) ]


toVertex (Pnt x y)  =  GL.vertex $ GL.Vertex3
 (realToFrac x) (realToFrac y) (0::GL.GLfloat)



-- Definició de les comandes per moure el llapis

type Para      = ()
type Angle     = Float
type Distancia = Float
data Comanda   = Avanca Distancia
               | Gira Angle
               | Para
               | Comanda :#: Comanda
               | CanviaColor Llapis
               | Branca Comanda
                deriving (Eq)

-- Problema 8
-- Pas de comandes a lines a pintar per GL graphics

-- Executa una comanda i retorna una llista de línies
execute :: Comanda -> [Ln]
execute c =
  let c' = ajuntaNoPara (separaAmbPara c) -- Ajunta les comandes consecutives i elimina les comandes Para consecutives
  in go (marro, Pnt 0 0, 0) c'            -- Inicia l'estat inicial (llapis, punt inicial, angle) i crida la funció go
  where
    -- Funció auxiliar per executar les comandes recursivament
    go :: (Llapis, Pnt, Angle) -> Comanda -> [Ln]
    go _ Para = []                        -- Si la comanda és Para, no es generen línies i es retorna una llista buida
    -- Calcula el punt final basat en la distància i l'angle, utilitzant les funcions trigonomètriques
    go (pencil, start, angle) (Avanca dist :#: rest) =
    -- Genera una línia amb el llapis, el punt inicial i el punt final, i crida recursivament go amb l'estat actualitzat i la resta de la comanda
      let end = start + Pnt (dist * cos (angle * pi / 180)) (dist * sin (angle * pi / 180))
      in Ln pencil start end : go (pencil, end, angle) rest
    -- En cas que la comanda Avança no estigui seguida de cap altra comanda, s'acaba l'execució i s'obté una única línia
    go (pencil, start, angle) (Avanca dist) =
    -- Genera una única línia amb el llapis, el punt inicial i el punt final
      let end = start + Pnt (dist * cos (angle * pi / 180)) (dist * sin (angle * pi / 180))
      in [Ln pencil start end]
    -- Actualitza l'angle i crida recursivament go amb l'estat actualitzat i la resta de la comanda
    go state@(pencil, start, angle) (Gira newAngle :#: rest) =
      go (pencil, start, angle - newAngle) rest
    -- Si la comanda Gira no està seguida de cap altra comanda, s'acaba l'execució sense generar línies
    go state@(pencil, start, angle) (Gira newAngle) =
      go (pencil, start, angle - newAngle) Para
    -- Actualitza el llapis i crida recursivament go amb l'estat actualitzat i la resta de la comanda
    go (pencil, start, angle) (CanviaColor newPencil :#: rest) =
      go (newPencil, start, angle) rest
    -- Si la comanda CanviaColor no està seguida de cap altra comanda, s'acaba l'execució sense generar línies
    go (pencil, start, angle) (CanviaColor newPencil) =
      go (newPencil, start, angle) Para
    -- Executa la branca recursivament i obté les línies generades
    -- Concatena les línies de la branca amb les línies generades per les comandes següents i crida recursivament go
    go state@(pencil, start, angle) (Branca branch :#: rest) =
      let branchLines = go (pencil, start, angle) branch
      in branchLines ++ go (pencil, start, angle) rest
    -- Executa la branca recursivament i obté les línies generades
    -- Concatena les línies de la branca amb les línies generades per la resta de la comanda i crida recursivament go
    go state@(pencil, start, angle) (Branca branch) =
      let branchLines = go (pencil, start, angle) branch
      in branchLines ++ go (pencil, start, angle) Para
    -- Si la comanda no és cap de les anteriors, es descarta i es crida recursivament go amb la resta de la comanda
    go state@(pencil, start, angle) (_ :#: rest) =
      go state rest


separa :: Comanda -> [Comanda]
separa (Para) = []
separa (c1 :#: c2) = separa c1 ++ separa c2
separa c = [c]

separaAmbPara :: Comanda -> [Comanda]
separaAmbPara c@(Para :#: rest) = c : separaAmbPara rest
separaAmbPara (c1 :#: c2) = separaAmbPara c1 ++ separaAmbPara c2
separaAmbPara c = [c]

-- Problema 2

ajunta :: [Comanda] -> Comanda
ajunta [] = Para
ajunta cs = foldr1 (:#:) (cs ++ [Para])

ajuntaNoPara :: [Comanda] -> Comanda
ajuntaNoPara [] = Para
ajuntaNoPara cs = foldr1 (:#:) cs

-- Rescales all points in a list of lines
--  from an arbitrary scale
--  to (-1.-1) - (1.1)

rescale :: [Ln] -> [Ln]
rescale lines | points == [] = []
              | otherwise    = map f lines
  where
  f (Ln pencil p q)  =  Ln pencil (g p) (g q)
  g p             =  swap ((p - p0) / s)
  points          =  [ r | Ln pencil p q <- lines, r <- [p, q] ]
  hi              =  foldr1 lub points
  lo              =  foldr1 glb points
  s               =  scalarMax (hi - lo) * scalar (0.55)
  p0              =  (hi + lo) * scalar (0.5)
  swap (Pnt x y)  =  Pnt y x


-- Generators for QuickCheck

instance Show Comanda where
  show Para = "Para"
  show (Avanca dist) = "Avanca " ++ show dist
  show (Gira angle)
    | angle < 0 = "Gira (" ++ show angle ++ ")"
    | otherwise = "Gira " ++ show angle
  show (CanviaColor color) = "CanviaColor " ++ show color
  show (Branca comanda) = "Branca (" ++ show comanda ++ ")"
  show (c1 :#: c2) = show c1 ++ " :#: " ++ show c2

instance Arbitrary Llapis where
    arbitrary  =  sized pencil
        where
          pencil n  =  elements [negre,vermell,verd,blau,blanc,Transparent]


instance Arbitrary Comanda where
    arbitrary  =  sized cmd
        where
          cmd n  |  n <= 0     =  oneof [liftM (Avanca . abs) arbitrary,
                                         liftM Gira arbitrary ]
                 |  otherwise  =  liftM2 (:#:) (cmd (n `div` 2)) (cmd (n `div`2))
