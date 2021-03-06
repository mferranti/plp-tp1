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
  "repeticionesPromedio" ~: testsRepeticionesPromedio,
  "frecuenciaTokens" ~: testsFrecuenciaTokens,
  "normalizarExtractor" ~: testsNormalizarExtractor,
  "extraerFeatures" ~: testsExtraerFeatures,
  "distEuclideana" ~: testsDistEuclideana,
  "distCoseno" ~: testsDistCoseno,
  "knn" ~: testsKnn,
  "separarDatos" ~: testsSepararDatos,
  "accuracy" ~: testsAccuracy
  ]

testsSplit = test [
  split ',' ",PLP," ~?= ["PLP"],
  split ',' " ,PLP, " ~?= [" ","PLP"," "],
  split ',' "hola PLP, bienvenidos!" ~?= ["hola PLP"," bienvenidos!"]
    ]

testsCuentas = test [
  cuentas ["x","x","y","x","z"] ~?= [(3,"x"), (1,"y"), (1,"z")],
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

testsFrecuenciaTokens = test [
  (frecuenciaTokens !! 25) "@a@a" ~?= 0.5,
  (frecuenciaTokens !! 26) "?abcdefgh?" ~?= 0.2,
  (head frecuenciaTokens) "___" ~?= 1,
  (head frecuenciaTokens) "abcde fghi" ~?= 0,
  (head frecuenciaTokens) "use_snake_case !" ~?= 0.125
  ] 

testsNormalizarExtractor = test [
    (normalizarExtractor ["hola", "pedrito", "0123456789"] (\text -> realToFrac(length text))) "hola" ~?= 4/10,
    (normalizarExtractor ["hola", "pedrito", "0123456789"] (\text -> realToFrac(length text))) "pedrito" ~?= 7/10,
    (normalizarExtractor ["hola", "pedrito", "0123456789"] (\text -> realToFrac(length text))) "0123456789" ~?= 1
  ] 

testsExtraerFeatures = test [
    extraerFeatures [longitudPromedioPalabras, repeticionesPromedio] ["b=a", "a = 2; a = 4", "C:/DOS C:/DOS/RUN RUN/DOS/RUN"] ~?= [[0.33333334,0.6666667],[0.12962963,1.0],[1.0,0.6666667]]
  ]

testsDistEuclideana = test [
    distEuclideana [1,1] [1,1] ~?= 0,
    distEuclideana [4,4] [3,3] ~?= sqrt 2,
    distEuclideana [1.0,0.75,0.8125] [0.75,1.0,0.5] ~?= 0.47186464
  ] 

testsDistCoseno = test [
    distCoseno [1,1] [1,0] ~?= 0.70710677,
    distCoseno [20,20] [20,0] ~?= 0.70710677,
    distCoseno [0,3,4] [0,-3,-4] ~?= -1.0
  ]

testsKnn = test [
    (knn 1 [[0,0],[2,2]] ["a","b"] distEuclideana) [1,1] ~?= "a",
    (knn 1 [[0,0],[0,0]] ["a","b"] distEuclideana) [1,1] ~?= "a",
    (knn 1 [[-3,-3],[0,0]] ["a","b"] distEuclideana) [0,0] ~?= "b",
    (knn 1 [[0,1],[0,2],[1,0]] ["a","a","b"] distEuclideana) [2,2] ~?= "a",
    (knn 1 [[0,1],[0,2],[1,0]] ["a","a","b"] distEuclideana) [3,1] ~?= "b",
    (knn 2 [[0,1],[0,2],[2,1],[1,1],[2,3]] ["i","i","f","f","i"] distEuclideana) [1,1] ~?= "f"
  ]

testsSepararDatos = test [
    separarDatos [[0,0],[1,1]] ["1","2"] 2 1 ~?= ([[1.0,1.0]],[[0.0,0.0]],["2"],["1"]),
    separarDatos [[1,1],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7]] ["1","2","3","4","5","6","7"] 3 2 ~?= ([[1.0,1.0],[2.0,2.0],[5.0,5.0],[6.0,6.0]],[[3.0,3.0],[4.0,4.0]],["1","2","5","6"],["3","4"])
  ]

testsAccuracy = test [
   accuracy ["f"] ["i", "f"] ~?= 0,
   accuracy ["f"] ["f"] ~?= 1,
   accuracy ["f","i","i"] ["f","f","f"] ~?= 1/3,
   accuracy ["f", "f", "i", "i", "f"] ["i", "f", "i", "f", "f"] ~?= 0.6
  ]
