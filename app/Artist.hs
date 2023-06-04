module Artist (
  ajunta,
  separa,
  ajuntaNoPara,
  separaAmbPara,
  optimitza,
  copia,
  poligon,
  pentagon,
  triangle,
  fulla,
  hilbert,
  fletxa,
  branca,
  bush,
  flor,
  espiral,
  prop_equivalent,
  prop_split,
  prop_split_join,
  prop_poligon_pentagon,
  triangleGrammar,
  fletxaGrammar,
  hilbertGrammar,
  brancaGrammar,
  bushGrammar,
  florGrammar,
  fullaGrammar,
  comanda_debug,
  lsystem
) where

import Test.QuickCheck
import Debug.Trace
import Data.Maybe
import UdGraphic

-- Problema 1

separa :: Comanda -> [Comanda]
separa (Para) = []                          -- La comanda "Para" retorna una llista buida ([]).
separa (c1 :#: c2) = separa c1 ++ separa c2 -- La comanda composta per c1 i c2 s'expandeix en les seves parts separades.
separa c = [c]                              -- Qualsevol altra comanda retorna una llista que conté només la mateixa comanda.


separaAmbPara :: Comanda -> [Comanda]
separaAmbPara c@(Para :#: rest) = c : separaAmbPara rest          -- Si la comanda és de la forma "Para :#: rest", s'afegeix a la llista i es crida recursivament a "separaAmbPara" amb la part "rest".
separaAmbPara (c1 :#: c2) = separaAmbPara c1 ++ separaAmbPara c2  -- Si la comanda és de la forma "c1 :#: c2", es crida recursivament a per a les dues parts de la comanda i s'uneixen els resultats en una sola llista.
separaAmbPara c = [c]                                             -- Qualsevol altra comanda retorna una llista que conté només la mateixa comanda.


-- Problema 2

ajunta :: [Comanda] -> Comanda
ajunta [] = Para                        -- Si la llista de comandes és buida, retorna "Para".
ajunta cs = foldr1 (:#:) (cs ++ [Para]) -- Uneix les comandes de la llista amb l'operador compost ":#:".
                                        -- Afegeix "Para" com a última comanda per separar les parts.

ajuntaNoPara :: [Comanda] -> Comanda
ajuntaNoPara [] = Para                  -- Si la llista de comandes és buida, retorna "Para".
ajuntaNoPara cs = foldr1 (:#:) cs       -- Uneix les comandes de la llista amb l'operador ":#:".
                                        -- No s'afegeix cap "Para" extra al final.

-- Problema 3

-- Compara si les comandes comanda1 i comanda2 són equivalents (iguals) i si el seu ajuntament (ajunta) també és equivalent.
prop_equivalent :: Comanda -> Comanda -> Property
prop_equivalent comanda1 comanda2 =
  (comanda1 == comanda2) === (ajunta [comanda1] == ajunta [comanda2])

-- Comprova si el resultat d'ajunta de les comandes separades de c és igual a c mateix. Aquesta propietat verifica si
-- separa i ajunta són operacions inverses.
prop_split_join :: Comanda -> Property
prop_split_join c = ajunta (separa c) === c

-- Comprova si la llista de comandes separades de c (usant separa) no conté cap comanda "Para" o una comanda composta.
-- Aquesta propietat verifica que la funció separa només produeix comandes individuals (no "Para" ni comandes compostes).
prop_split :: Comanda -> Property
prop_split c =
  not (any isParaOrConc (separa c)) === True
  where
    isParaOrConc Para = True
    isParaOrConc (_ :#: _) = True
    isParaOrConc _ = False

-- Problema 4

-- Utilitza la funció replicate per a crear una llista de n repeticions de la comanda c,
-- i després utilitza ajuntaNoPara per a unir aquestes comandes en una sola comanda.
copia :: Int -> Comanda -> Comanda
copia n c = ajuntaNoPara (replicate n c)

-- Problema 5

-- Utilitza la funció copia per a crear una comanda que consisteix en copiar 5 vegades la seqüència d'Avanca d i Gira 72.
-- Això representa la creació d'un pentàgon regular amb costat de longitud d.
pentagon :: Distancia -> Comanda
pentagon d = copia 5 (Avanca d :#: Gira 72)

-- Problema 6

--Utilitza la funció copia per a crear una comanda que consisteix en copiar l vegades la seqüència d'Avanca d i Gira a.
-- Això permet la creació d'un polígon regular amb n costats (on n és el valor de l), cada costat de longitud d i un gir
-- de l'angle a.
poligon :: Distancia -> Int -> Angle -> Comanda
poligon d l a = copia l (Avanca d :#: Gira a)

-- Aquesta propietat verifica si el polígon creat utilitzant la funció poligon amb els paràmetres d, s i a és equivalent
-- al pentàgon creat amb la funció pentagon utilitzant el mateix valor de d.
-- És a dir, comprova si les dues funcions generen la mateixa comanda per a les mateixes distàncies i angles.
prop_poligon_pentagon :: Distancia -> Int -> Angle -> Property
prop_poligon_pentagon d s a = poligon d s a === pentagon d

-- Problema 7

-- Utilitza zipWith per combinar els índexs i els comptadors
-- S'utilitza una funció anònima que pren un element de la llista d'índexs i un element de la llista de comptadors i genera una comanda de polígon basada en aquests valors.
espiral :: Distancia -> Int -> Distancia -> Angle -> Comanda
espiral len num pas ang = ajuntaNoPara $ zipWith (\i counter -> poligon (len + fromIntegral i * pas) 1 ang) [0..] [1..num]

-- Problema 9

optimitza :: Comanda -> Comanda
optimitza = ajuntaNoPara . opt 0 0 False . separa
  where
    -- Implementació de la funció optimitza utilitzant les funcions auxiliars opt, emitAvanca, emitGira i para
    opt :: Float -> Float -> Bool -> [Comanda] -> [Comanda]
    -- Condició de parada: si no queden més comandes, es crida la funció para per generar la comanda final
    opt a g nonZero [] = para a g nonZero
    -- Si es troba una comanda Para, s'ignora i es crida opt recursivament amb la resta de comandes
    opt a g nonZero (Para : cs) = opt a g nonZero cs
    -- Si es troba una comanda Avança 0 o Gira 0, s'ignora i es crida opt recursivament amb la resta de comandes
    opt a g nonZero (Avanca 0 : cs) = opt a g nonZero cs
    opt a g nonZero (Gira 0 : cs) = opt a g nonZero cs
    -- Si es troba una comanda Avança d diferent de zero, s'afegeix l'angle acumulat g a través de la funció emitGira
    -- i es crida opt recursivament amb la nova distància acumulada a+d, l'angle reiniciat 0 i l'indicador True
    opt a g _ (Avanca d : cs) = emitGira g $ opt (a+d) 0 True cs
    -- Si es troba una comanda Gira d diferent de zero, s'afegeix la distància acumulada a través de la funció emitAvanca
    -- i es crida opt recursivament amb la distància reiniciada 0, l'angle acumulat g+d i l'indicador True
    opt a g _ (Gira d : cs) = emitAvanca a $ opt 0 (g+d) True cs
    -- Si es troba una comanda CanviaColor l, aquesta es conserva i es crida opt recursivament amb la resta de comandes
    opt a g nonZero ((CanviaColor l) : cs) = CanviaColor l : opt a g nonZero cs
    -- Si es troba una comanda Branca c, s'afegeix la distància acumulada a través de la funció emitAvanca,
    -- l'angle acumulat g a través de la funció emitGira, i es crida optimitza recursivament amb la comanda c
    -- optimitzada (optimitza c), i s'afegeix a la llista de comandes Branca
    -- Finalment, es crida opt recursivament amb la distància i l'angle reiniciats (0), l'indicador posat a True
    -- i la resta de comandes cs
    opt a g nonZero ((Branca c) : cs) = emitAvanca a $ emitGira g $ Branca (optimitza c) : opt 0 0 True cs

    -- Funció auxiliar que afegeix una comanda Avança amb una distància a la llista de comandes
    emitAvanca :: Float -> [Comanda] -> [Comanda]
    emitAvanca 0 cs = cs
    emitAvanca a cs = Avanca a : cs

    -- Funció auxiliar que afegeix una comanda Gira amb un angle a la llista de comandes
    emitGira :: Float -> [Comanda] -> [Comanda]
    emitGira 0 cs = cs
    emitGira g cs = Gira g : cs

    -- Funció auxiliar que genera la comanda Para o una seqüència d'Avança i Gira amb les distàncies i angles proporcionats
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

-- Utilitza la funció `lookup` per obtenir el valor associat a la clau `s` dins dels `rules`.
-- En cas que no s'hi trobi, retorna [s] com a valor per defecte.
applyRules :: Rules -> Symbol -> String
applyRules rules s = fromMaybe [s] (lookup s rules)

gen :: Rules -> Int -> String -> String
-- Condició de parada: si `n` és 0, retorna la cadena `s`
gen rules 0 s = s
-- Genera una nova cadena aplicant repetidament les regles a cada símbol de la cadena `s`.
-- A cada iteració, el nombre `n` es redueix en 1.
gen rules n s = gen rules (n-1) (concatMap (applyRules rules) s)

-- Utilitza la funció `gen` per generar la cadena de símbols i `rewrite` per convertir la cadena en una comanda,
-- tot passant per la funció `optimitza` per optimitzar la comanda resultant.
comanda :: Grammar -> Int -> Comanda
comanda grammar n = optimitza $ (rewrite grammar) grammar (gen (rules grammar) n (start grammar))

-- Auxiliar per obtenir el L-system sense reescriptura
lsystem :: Grammar -> Int -> String
lsystem grammar n = gen (rules grammar) n (start grammar)

-- Auxiliar per obtenir el L-system amb reescriptura no optimitzada
comanda_debug :: Grammar -> Int -> Comanda
comanda_debug grammar n = (rewrite grammar) grammar (gen (rules grammar) n (start grammar))

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
    rules = [('f', "g+f+g"), ('g', "f-g-f")],
    start = "f",
    rewrite = replaceCommandsFletxa
}

brancaGrammar :: Grammar
brancaGrammar = Grammar {
    angle = 22.5,
    rules = [('g', "f-[[g]+g]+f[+fg]-g"), ('f', "ff")],
    start = "g",
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
-- Reemplaçen els símbols de la cadena segons les regles

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
                    in  (Avanca 1 :#: cmds, rest)
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