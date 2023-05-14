module Artist where

import Internal
import Test.QuickCheck
import Debug.Trace
import Data.Maybe

-- Problema 1

separa :: Comanda -> [Comanda]
separa c@(Para) = []
separa (c1 :#: c2) = separa c1 ++ separa c2
separa c@(Avanca _) = [c]
separa c@(Gira _) = [c]

-- Problema 2

ajunta :: [Comanda] -> Comanda
ajunta [c] = c :#: Para
ajunta (c:r) = c :#: ajunta r

-- Problema 3

prop_equivalent :: Comanda -> Comanda -> Property
prop_equivalent comanda1 comanda2 =
  (comanda1 == comanda2) === (ajunta [comanda1] == ajunta [comanda2])

prop_split_join :: Comanda -> Property
prop_split_join c = ajunta (separa c) === c

prop_split :: Comanda -> Property
prop_split c =
  not (any isPara (separa c)) .&&. not (any isConc (separa c))
  where
    isPara Para = True
    isPara _ = False
    isConc (_ :#: _) = True
    isConc _ = False

-- Problema 4

copia :: Int -> Comanda -> Comanda
copia n c = ajuntaNoPara (replicate n c)

ajuntaNoPara :: [Comanda] -> Comanda
ajuntaNoPara [c] = c
ajuntaNoPara (c:r) = c :#: ajuntaNoPara r

-- Problema 5

pentagon :: Distancia -> Comanda
pentagon d = copia 5 (Avanca d :#: Gira 72)

-- Problema 6

poligon :: Distancia -> Int -> Angle -> Comanda
poligon d l a = copia l (Avanca d :#: Gira a)

prop_poligon_pentagon :: Distancia -> Int -> Angle -> Property
prop_poligon_pentagon d s a = poligon d s a === pentagon d

-- Problema 7

espiral :: Distancia -> Int -> Distancia -> Angle -> Comanda
espiral len num pas ang = ajuntaNoPara $ zipWith (\iteration counter -> poligon (len + fromIntegral iteration * pas) 1 ang) [0..] counter_list
  where
    counter_list = take num [1..]

-- Problema 9

optimitza :: Comanda -> Comanda
optimitza = ajuntaNoPara . opt 0 0 False . separa
  where
    opt :: Float -> Float -> Bool -> [Comanda] -> [Comanda]
    opt a g nonZero [] = para a g nonZero
    opt a g nonZero (Para : cs) = opt a g nonZero cs
    opt a g nonZero (Avanca 0 : cs) = opt a g nonZero cs
    opt a g nonZero (Gira 0 : cs) = opt a g nonZero cs
    opt a g _ (Avanca d : cs) = emitGira g $ opt (a+d) 0 True cs
    opt a g _ (Gira d : cs) = emitAvanca a $ opt 0 (g+d) True cs
    emitAvanca :: Float -> [Comanda] -> [Comanda]
    emitAvanca 0 cs = cs
    emitAvanca a cs = Avanca a : cs
    emitGira :: Float -> [Comanda] -> [Comanda]
    emitGira 0 cs = cs
    emitGira g cs = Gira g : cs
    para :: Float -> Float -> Bool -> [Comanda]
    para 0 0 False = [Para]
    para a g _ = emitAvanca a $ emitGira g []

-- Problema 10

triangle :: Int -> Comanda
triangle = undefined

-- Problema 11

fulla :: Int -> Comanda
fulla = undefined

-- Problema 12

hilbert :: Int -> Comanda
hilbert = undefined

-- Problema 13

fletxa :: Int -> Comanda
fletxa = undefined

-- Problema 14

branca :: Int -> Comanda
branca = undefined
