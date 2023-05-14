module Internal (
    Comanda(..),
    Distancia,
    Angle
) where


import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT hiding (Angle)
infixr 5 :#:

data Llapis = Color' GL.GLfloat GL.GLfloat GL.GLfloat
            | Transparent
            deriving (Eq, Ord, Show)

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

instance Show Comanda where
  show (Avanca d) = "Avanca " ++ show d
  show (Gira a) = "Gira " ++ show a
  show (Para) = "Para"
  show (c1 :#: c2) = show c1 ++ " :#: " ++ show c2