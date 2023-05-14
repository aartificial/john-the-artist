module Internal (
    Comanda(..),
    Distancia,
    Angle
) where

infixr 5 :#:

type Para      = ()
type Angle     = Float
type Distancia = Float
data Comanda   = Avanca Distancia
               | Gira Angle
               | Para
               | Comanda :#: Comanda
                deriving (Eq)

instance Show Comanda where
  show (Avanca d) = "Avanca " ++ show d
  show (Gira a) = "Gira " ++ show a
  show (Para) = "Para"
  show (c1 :#: c2) = show c1 ++ " :#: " ++ show c2