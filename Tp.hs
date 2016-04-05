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
cuentas = foldr (\x r ->
                    if (elem x (snd (unzip r) ))
                    then map (\y ->
                          if (x == (snd y))
                          then (1 + fst y, snd y)
                          else y
                        )
                        r
                    else (1,x):r
                )
                []

-- Ejercicio 4
-- Implementar repeticionPromedio :: Extractor que calcula la cantidad promedio de repeticiones por cada palabra

repeticionesPromedio :: Extractor
repeticionesPromedio = \text -> let listPalabras = split ' ' text in
                                (fromIntegral (genericLength listPalabras)) / fromIntegral (genericLength (cuentas listPalabras))

tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"


-- Ejercicio 5
-- Implementar frecuenciasTokens :: [Extractor] que devuelve un extractor por cada uno de
-- los sı́mbolos definidos en la constante tokens :: [Char] 2 con la frecuencia relativa de estos con
-- respecto a todos los caracteres presentes en el programa.

frecuenciaTokens :: [Extractor]
frecuenciaTokens = [ \text -> (frecuenciaT t (cuentas text)) / fromIntegral (length text) | t <- tokens ]

frecuenciaT :: Char -> [(Int,Char)] -> Feature
frecuenciaT = \t ls -> let k = filter (\x -> t == (snd x)) ls in
                 if (length k == 0)
                 then (fromIntegral 0)
                 else (fromIntegral (fst (head k)))

--Ejercicio 6
-- normalizarExtractor :: [Texto] -> Extractor -> Extractor que dado un extractor, lo “modifica”
-- de manera que el valor de los features se encuentre entre -1 y 1 para todos los datos con los que
-- se dispone. Por ejemplo, suponiendo los textos t1, t2 y t3 y un extractor que devuelve los valores
-- -20.3, 1.0 y 10.5 respectivamente, deberı́a ser modificado para que al aplicarlo nuevamente a esos
-- textos, los valores sean -1.0, 0.04 y 0.51.

-- #TODO chequear la normalizacion. 
normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor ts extractor = \text -> snd (head (filter (\x -> (fst x) == text) (zip ts (normalizar datos)) ) )
                                      where datos = (map extractor ts)

normalizar :: [Feature] -> [Feature]
normalizar datos = map (\x -> x / (norma datos) ) datos

norma :: [Feature] -> Float
norma datos = maximum (map abs datos)


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
