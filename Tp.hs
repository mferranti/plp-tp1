module Tp where

import Data.List

type Texto = String
type Feature = Float
type Instancia = [Feature]
type Extractor = (Texto -> Feature)

type Datos = [Instancia]
type Etiqueta = String
type Modelo = (Instancia -> Etiqueta)
type Medida = (Instancia -> Instancia -> Float)

tryClassifier :: [Texto] -> [Etiqueta] -> Float
tryClassifier x y = let xs = extraerFeatures ([longitudPromedioPalabras, repeticionesPromedio] ++ frecuenciaTokens) x in
    nFoldCrossValidation 5 xs y

mean :: [Float] -> Float
mean xs = realToFrac (sum xs) / genericLength xs

-- Ejercicio 1
-- Implementar la función auxiliar split :: Eq a => a -> [a] -> [[a]] Dado un elemento separador y una lista,
-- se deberá partir la lista en sublistas de acuerdo a la aparición del separador (sin incluirlo)
 
split :: Eq a => a -> [a] -> [[a]]
split a xs = if length xs /= 0 then split' a xs else []

split' :: Eq a => a -> [a] -> [[a]]
split' needle = foldr (\x r -> if (x /= needle) then (x:head r):(tail r) else []:r) [[]]

-- Ejercicio 2
-- Implementar longitudPromedioPalabras :: Extractor , que dado un texto, calcula la longitud
-- promedio de sus palabras. Consideraremos palabra a cualquier secuencia de caracteres separadas por espacios

longitudPromedioPalabras :: Extractor
longitudPromedioPalabras = \text -> mean (map genericLength (split ' ' text))

-- Ejercicio 3
-- Implementar la función auxiliar: cuentas :: Eq a => [a] -> [(Int, a)] que dada una lista,
-- deberá devolver la cantidad de veces que aparece cada elemento en la lista. 

cuentas :: Eq a => [a] -> [(Int, a)]
cuentas = undefined

repeticionesPromedio :: Extractor
repeticionesPromedio = undefined

tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

frecuenciaTokens :: [Extractor]
frecuenciaTokens = undefined

normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor = undefined

extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures = undefined

distEuclideana :: Medida
distEuclideana = undefined

distCoseno :: Medida
distCoseno = undefined

knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn = undefined

accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy = undefined

separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos = undefined

nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation = undefined
