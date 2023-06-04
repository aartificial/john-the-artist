module Main where

import Artist
import UdGraphic
import Test.QuickCheck
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT hiding (Angle)

main :: IO ()
main = display (fulla 3)
-- main = display (espiral 30 100 5 30)
-- main = display (triangle 7)
-- main = display (fulla 7)
-- main = display (hilbert 6)
-- main = display (fletxa 12)
-- main = display (branca 12)
-- main = display (bush 12)
-- main = display (flor 20)

test1 :: IO ()
test1 = print $ separa(Avanca 3 :#: Gira 4 :#: Avanca 7 :#: Para)

test2 :: IO ()
test2 = print $ ajunta [Avanca 3, Gira 4, Avanca 7]

test3 :: IO ()
test3 = quickCheck $ prop_equivalent Para Para

test3_1 :: IO ()
test3_1 = quickCheck $ prop_split_join $ Avanca 40 :#: Gira 30 :#: Avanca 20 :#: Avanca 10 :#: Para

test3_2 :: IO ()
test3_2 = quickCheck $ prop_split $ Avanca 40 :#: Gira 30 :#: Avanca 20 :#: Avanca 10 :#: Para

test4 :: IO ()
test4 = print $ copia 3 $ Avanca 10 :#: Gira 120

test5 :: IO ()
test5 = print $ pentagon 50

test6 :: IO ()
test6 = print $ poligon 50 5 72

test6_1 :: IO ()
test6_1 = quickCheck $ prop_poligon_pentagon 50 5 72

test7 :: IO ()
test7 = print $ espiral 30 4 5 30

test7_1 :: IO ()
test7_1 = print $ espiral 30 0 5 30

test7_2 :: IO ()
test7_2 = print $ espiral 30 4 (-5) 30

test7_3 :: IO ()
test7_3 = print $ espiral 40 20 (-5) 30

test8 :: IO ()
test8 = print $ execute $ (Avanca 30 :#: Para :#: Gira 10) :#: Avanca 20

test8_1 :: IO ()
test8_1 = print $ execute $ Avanca 30 :#: Para :#: Gira 10 :#: Avanca 20 :#: Gira (-15) :#: Para :#: Avanca 10 :#: Para :#: Para

test9 :: IO ()
test9 = print $ optimitza $ Avanca 10 :#: Para :#: Avanca 20 :#: Gira 35 :#: Avanca 0 :#: Gira 15 :#: Gira(-50)

test9_1 :: IO ()
test9_1 = print $ optimitza $ Gira 10 :#: Gira 0 :#: Avanca 0 :#: Gira 0 :#: Avanca 40 :#: Gira 30

test9_2 :: IO ()
test9_2 = print $ optimitza $ Avanca 10 :#: Avanca 0 :#: Gira 0 :#: Avanca 0 :#: Gira 40 :#: Avanca 30

test9_3 :: IO ()
test9_3 = print $ optimitza $ Avanca 10 :#: Avanca 0 :#: Avanca (-60)

test9_4 :: IO ()
test9_4 = print $ optimitza $ Avanca 0 :#: Gira 0 :#: Avanca 0 :#: Para

test9_5 :: IO ()
test9_5 = print $ optimitza $ Avanca 50 :#: Gira 123 :#: Avanca (-50) :#: Gira (-123)

test9_6 :: IO ()
test9_6 = print $ optimitza $ Avanca 50 :#: Gira 0 :#: Avanca (-50) :#: Gira (-123)

test9_7 :: IO ()
test9_7 = print $ optimitza $ Avanca 50 :#: Gira 0 :#: Avanca (-50) :#: Gira(-123) :#: Avanca 50

test10_1 :: IO ()
test10_1 = print $ lsystem triangleGrammar 1
test10_2 :: IO ()
test10_2 = print $ lsystem triangleGrammar 2
test10_3 :: IO ()
test10_3 = print $ lsystem triangleGrammar 3

test11_1 :: IO ()
test11_1 = print $ lsystem fullaGrammar 1
test11_2 :: IO ()
test11_2 = print $ lsystem fullaGrammar 2
test11_3 :: IO ()
test11_3 = print $ lsystem fullaGrammar 3

test12_1 :: IO ()
test12_1 = print $ lsystem hilbertGrammar 1
test12_2 :: IO ()
test12_2 = print $ lsystem hilbertGrammar 2
test12_3 :: IO ()
test12_3 = print $ lsystem hilbertGrammar 3

test13_1 :: IO ()
test13_1 = print $ lsystem fletxaGrammar 1
test13_2 :: IO ()
test13_2 = print $ lsystem fletxaGrammar 2
test13_3 :: IO ()
test13_3 = print $ lsystem fletxaGrammar 3

test14_1 :: IO ()
test14_1 = print $ lsystem brancaGrammar 1
test14_2 :: IO ()
test14_2 = print $ lsystem brancaGrammar 2
test14_3 :: IO ()
test14_3 = print $ lsystem brancaGrammar 3