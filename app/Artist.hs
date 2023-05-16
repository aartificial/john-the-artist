module Artist (
  ajunta,
  separa,
  ajuntaNoPara,
  separaAmbPara,
  optimitza,
  pentagon,
  triangle,
  fulla,
  hilbert,
  fletxa,
  branca,
  bush,
  flor
) where

import Test.QuickCheck
import Debug.Trace
import Data.Maybe
import UdGraphic

-- Problema 1

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

-- Problema 3

prop_equivalent :: Comanda -> Comanda -> Property
prop_equivalent comanda1 comanda2 =
  (comanda1 == comanda2) === (ajunta [comanda1] == ajunta [comanda2])

prop_split_join :: Comanda -> Property
prop_split_join c = ajunta (separa c) === c

prop_split :: Comanda -> Property
prop_split c =
  not (any isParaOrConc (separa c)) === True
  where
    isParaOrConc Para = True
    isParaOrConc (_ :#: _) = True
    isParaOrConc _ = False

-- Problema 4

copia :: Int -> Comanda -> Comanda
copia n c = ajuntaNoPara (replicate n c)

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
espiral len num pas ang = ajuntaNoPara $ zipWith (\i counter -> poligon (len + fromIntegral i * pas) 1 ang) [0..] [1..num]

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
    opt a g nonZero ((CanviaColor l) : cs) = CanviaColor l : opt a g nonZero cs
    opt a g nonZero ((Branca c) : cs) = emitAvanca a $ emitGira g $ Branca (optimitza c) : opt 0 0 False cs

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

-- Type definitions
type Symbol = Char
type Rule = (Symbol, String)
type Rules = [Rule]
type Rewrite = Grammar -> String -> Comanda

data Grammar = Grammar {
    angle :: Float,
    rules :: Rules,
    start :: String,
    rewrite :: Rewrite
}

applyRules :: Rules -> Symbol -> String
applyRules rules s = fromMaybe [s] (lookup s rules)

gen :: Rules -> Int -> String -> String
gen rules 0 s = s
gen rules n s = gen rules (n-1) (concatMap (applyRules rules) s)

comanda :: Grammar -> Int -> Comanda
comanda grammar n = optimitza $ (rewrite grammar) grammar (gen (rules grammar) n (start grammar))

-- Grammar definitions

triangleGrammar :: Grammar
triangleGrammar = Grammar {
    angle = 90,
    rules = [('f', "f+f-f-f+f")],
    start = "+f",
    rewrite = replaceCommandsTriangle
}

fullaGrammar :: Grammar
fullaGrammar = Grammar {
    angle = 45,
    rules = [('f', "g[-f][+f][gf]"), ('g', "gg")],
    start = "f",
    rewrite = replaceCommandsFulla
}

hilbertGrammar :: Grammar
hilbertGrammar = Grammar {
    angle = 90,
    rules = [('l', "+rf-lfl-fr+"), ('r', "-lf+rfr+fl-")],
    start = "l",
    rewrite = replaceCommandsHilbert
}

fletxaGrammar :: Grammar
fletxaGrammar = Grammar {
    angle = 60,
    rules = [('f', "g+f+g"), ('g', "g-f-g")],
    start = "f",
    rewrite = replaceCommandsFletxa
}

brancaGrammar :: Grammar
brancaGrammar = Grammar {
    angle = 22.5,
    rules = [('g', "f-[[g]+g]+f[+fg]-g"), ('f', "ff")],
    start = "r",
    rewrite = replaceCommandsBranca
}

bushGrammar :: Grammar
bushGrammar = Grammar {
    angle = 25.7,
    rules = [('l', "r[-fff][+fff]fl"), ('r', "rfl[+r][-r]")],
    start = "l",
    rewrite = replaceCommandsBush
}

florGrammar :: Grammar
florGrammar = Grammar {
    angle = 12,
    rules = [ ('a', "fffffv[+++h][---q]~b"),
              ('b', "fffffv[+++h][---q]~c"),
              ('c', "fffffv[+++~a]~d"),
              ('d', "fffffv[+++h][---q]~e"),
              ('e', "fffffv[+++h][---q]~g"),
              ('g', "fffffv[---~a]~a"),
              ('h', "i~ff"),
              ('i', "~fff[--m]j"),
              ('j', "~fff[--n]k"),
              ('k', "~fff[--o]l"),
              ('l', "~fff[--p]"),
              ('m', "~fn"),
              ('n', "~fo"),
              ('o', "~fp"),
              ('p', "~f"),
              ('q', "r~f"),
              ('r', "~fff[++m]s"),
              ('s', "~fff[++n]t"),
              ('t', "~fff[++o]u"),
              ('u', "~fff[++p]"),
              ('v', "fv")
            ],
    start = "af",
    rewrite = replaceCommandsFlor
}

-- Parse functions

replaceCommandsTriangle :: Rewrite
replaceCommandsTriangle grammar xs = fst $ parse 0 xs
  where
    ang = angle grammar
    parse :: Int -> String -> (Comanda, String)
    parse _ [] = (Para, [])
    parse depth (x:xs)
      | x == 'f'  = let (cmds, rest) = parse depth xs
                    in  (Avanca 1 :#: cmds, rest)
      | x == '+'  = let (cmds, rest) = parse depth xs
                    in  (Gira ang :#: CanviaColor verd :#: cmds, rest)
      | x == '-'  = let (cmds, rest) = parse depth xs
                    in  (Gira (-ang) :#: CanviaColor violeta :#: cmds, rest)

replaceCommandsFulla :: Rewrite
replaceCommandsFulla grammar xs = fst $ parse 0 xs
  where
    ang = angle grammar
    parse :: Int -> String -> (Comanda, String)
    parse _ [] = (Para, [])
    parse depth (x:xs)
      | x == 'f'  = let (cmds, rest) = parse depth xs
                    in  (CanviaColor vermell :#: Avanca 1 :#: cmds, rest)
      | x == '+'  = let (cmds, rest) = parse depth xs
                    in  (Gira ang :#: cmds, rest)
      | x == '-'  = let (cmds, rest) = parse depth xs
                    in  (Gira (-ang) :#: cmds, rest)
      | x == '['  = let (branchCmds, rest) = parse (depth + 1) xs
                        (cmds, rest') = parse depth rest
                    in  (Branca branchCmds :#: cmds, rest')
      | x == ']'  = if depth > 0
                    then (Para, xs)
                    else error "Unmatched closing bracket"
      | x == 'g'  = let (cmds, rest) = parse depth xs
                    in  (CanviaColor blau :#: Avanca 1 :#: cmds, rest)
      | otherwise = parse depth xs

replaceCommandsHilbert :: Rewrite
replaceCommandsHilbert grammar xs = fst $ parse 0 xs
  where
    ang = angle grammar
    parse :: Int -> String -> (Comanda, String)
    parse _ [] = (Para, [])
    parse depth (x:xs)
      | x == 'f'  = let (cmds, rest) = parse depth xs
                    in  (Avanca 1 :#: cmds, rest)
      | x == 'l'  = let (cmds, rest) = parse depth xs
                    in  (CanviaColor negre :#: cmds, rest)
      | x == 'r'  = let (cmds, rest) = parse depth xs
                    in  (CanviaColor taronja :#: cmds, rest)
      | x == '+'  = let (cmds, rest) = parse depth xs
                    in  (Gira ang :#: cmds, rest)
      | x == '-'  = let (cmds, rest) = parse depth xs
                    in  (Gira (-ang) :#: cmds, rest)
      | otherwise = parse depth xs

replaceCommandsFletxa :: Rewrite
replaceCommandsFletxa grammar xs = fst $ parse 0 xs
  where
    ang = angle grammar
    parse :: Int -> String -> (Comanda, String)
    parse _ [] = (Para, [])
    parse depth (x:xs)
      | x == 'f'  = let (cmds, rest) = parse depth xs
                    in  (CanviaColor rosa :#: Avanca 1 :#: cmds, rest)
      | x == 'g'  = let (cmds, rest) = parse depth xs
                    in  (CanviaColor violeta :#: Avanca (1) :#: cmds, rest)
      | x == '+'  = let (cmds, rest) = parse depth xs
                    in  (Gira ang :#: cmds, rest)
      | x == '-'  = let (cmds, rest) = parse depth xs
                    in  (Gira (-ang) :#: cmds, rest)

replaceCommandsBranca :: Rewrite
replaceCommandsBranca grammar xs = fst $ parse 0 xs
  where
    ang = angle grammar
    parse :: Int -> String -> (Comanda, String)
    parse _ [] = (Para, [])
    parse depth (x:xs)
      | x == 'f'  = let (cmds, rest) = parse depth xs
                    in  (Avanca 1:#: cmds, rest)
      | x == '+'  = let (cmds, rest) = parse depth xs
                    in  (Gira ang :#: cmds, rest)
      | x == '-'  = let (cmds, rest) = parse depth xs
                    in  (Gira (-ang) :#: cmds, rest)
      | x == '['  = let (branchCmds, rest) = parse (depth + 1) xs
                        (cmds, rest') = parse depth rest
                    in  (Branca (CanviaColor groc_d :#: branchCmds :#: CanviaColor groc_D) :#: cmds, rest')
      | x == ']'  = if depth > 0
                    then (Para, xs)
                    else error "Unmatched closing bracket"
      | otherwise = parse depth xs

replaceCommandsBush :: Rewrite
replaceCommandsBush grammar xs = fst $ parse 0 xs
  where
    ang = angle grammar
    parse :: Int -> String -> (Comanda, String)
    parse _ [] = (Para, [])
    parse depth (x:xs)
      | x == 'f'  = let (cmds, rest) = parse depth xs
                    in  (Avanca 1 :#: cmds, rest)
      | x == 'l'  = let (cmds, rest) = parse depth xs
                    in  (CanviaColor verd_d :#: cmds, rest)
      | x == 'r'  = let (cmds, rest) = parse depth xs
                    in  (CanviaColor rosa :#: cmds, rest)
      | x == '+'  = let (cmds, rest) = parse depth xs
                    in  (Gira ang :#: cmds, rest)
      | x == '-'  = let (cmds, rest) = parse depth xs
                    in  (Gira (-ang) :#: cmds, rest)
      | x == '['  = let (branchCmds, rest) = parse (depth + 1) xs
                        (cmds, rest') = parse depth rest
                    in  (Branca (CanviaColor groc :#: branchCmds :#: CanviaColor verd) :#: cmds, rest')
      | x == ']'  = if depth > 0
                    then (Para, xs)
                    else error "Unmatched closing bracket"

replaceCommandsFlor :: Rewrite
replaceCommandsFlor grammar xs = fst $ parse 0 xs
  where
    ang = angle grammar
    parse :: Int -> String -> (Comanda, String)
    parse _ [] = (Para, [])
    parse depth (x:xs)
      | x == 'f'  = let (cmds, rest) = parse depth xs
                    in  (CanviaColor verd_d :#: Avanca 1 :#: cmds, rest)
      | x `elem` "labcdefghijklmnopqrstuv"
                  = let (cmds, rest) = parse depth xs
                    in  (cmds, rest)
      | x == '~'  = let (cmds, rest) = parse depth xs
                    in  (CanviaColor blanc :#: Avanca 1 :#: cmds, rest)
      | x == '+'  = let (cmds, rest) = parse depth xs
                    in  (Gira ang :#: cmds, rest)
      | x == '-'  = let (cmds, rest) = parse depth xs
                    in  (Gira (-ang) :#: cmds, rest)
      | x == '['  = let (branchCmds, rest) = parse (depth + 1) xs
                        (cmds, rest') = parse depth rest
                    in  (Branca (branchCmds :#: CanviaColor violeta) :#: cmds, rest')
      | x == ']'  = if depth > 0
                    then (Para, xs)
                    else error "Unmatched closing bracket"

-- Generators

-- Problema 10
triangle :: Int -> Comanda
triangle n = comanda triangleGrammar n

-- Problema 11
fulla :: Int -> Comanda
fulla n = comanda fullaGrammar n

-- Problema 12
hilbert :: Int -> Comanda
hilbert n = comanda hilbertGrammar n

-- Problema 13
fletxa :: Int -> Comanda
fletxa n = comanda fletxaGrammar n

-- Problema 14
branca :: Int -> Comanda
branca n = comanda brancaGrammar n

bush :: Int -> Comanda
bush n = comanda bushGrammar n

flor :: Int -> Comanda
flor n = comanda florGrammar n