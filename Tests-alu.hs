-- Para correr los tests:
-- ghc Tests-alu.hs && ./Tests-alu

import Tp
import Test.HUnit
import Data.List

-- evaluar main para correr todos los tests
main = runTestTT allTests

allTests = test [
 	"split" ~: testsSplit,
 	"cuentas" ~: testsCuentas,
  	"longitudPromedioPalabras" ~: testsLongitudPromedioPalabras,
  	"repeticionesPromedio" ~: testsRepeticionesPromedio
  ]

testsSplit = test [
 	split ',' ",PLP," ~?= ["PLP"],
 	split ',' " ,PLP, " ~?= [" ","PLP"," "]
  	]

testsCuentas = test [
	cuentas ["x","x","y","x","z"] ~?= [(3,"x"), (1,"y"), (1,"z")],
--san
	cuentas["jose", "pablo","jose","armando","jose"] ~?= [(3,"jose"), (1,"pablo"), (1,"armando")],
	cuentas["1","22","22","333","333","333"] ~?= [(1,"1"), (2,"22"), (3,"333")]
	]

testsLongitudPromedioPalabras = test [
  longitudPromedioPalabras "" ~?= 0.0,
  longitudPromedioPalabras "Test" ~?= 4.0,
  longitudPromedioPalabras "Test longitud promedio palabras" ~?= 7.0,
  longitudPromedioPalabras "Este test tiene palabras $$++$$" ~?= 5.4
  ] 

testsRepeticionesPromedio = test [
  repeticionesPromedio "" ~?= 0.0,
  repeticionesPromedio "Test" ~?= 1.0,
  repeticionesPromedio "Test repeticion promedio palabras" ~?= 1.0,
  repeticionesPromedio "lalala $$++$$ lalala lalala $$++$$" ~?= 2.5,
  repeticionesPromedio "a a a " ~?= 3.0
  ] 

testsfrecuenciaTokens = test [
  (frecuenciaTokens !! 25) "@a@a" ~?= 0.5,
  (frecuenciaTokens !! 26) "?abcdefgh?" ~?= 0.2,
  (head frecuenciaTokens) "___" ~?= 1,
  (head frecuenciaTokens) "abcde fghi" ~?= 0
  ] 
