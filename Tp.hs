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

-- Ejercicio 6
-- normalizarExtractor :: [Texto] -> Extractor -> Extractor que dado un extractor, lo “modifica”
-- de manera que el valor de los features se encuentre entre -1 y 1 para todos los datos con los que
-- se dispone. Por ejemplo, suponiendo los textos t1, t2 y t3 y un extractor que devuelve los valores
-- -20.3, 1.0 y 10.5 respectivamente, deberı́a ser modificado para que al aplicarlo nuevamente a esos
-- textos, los valores sean -1.0, 0.04 y 0.51.

normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor ts extractor = \text -> snd (head (filter (\x -> (fst x) == text) (zip ts (normalizar datos)) ) )
                                      where datos = (map extractor ts)

normalizar :: [Feature] -> [Feature]
normalizar datos = map (\x -> x / (norma1 datos) ) datos

norma1 :: [Feature] -> Float
norma1 datos = maximum (map abs datos)

norma2 :: [Feature] -> Float
norma2 datos = sqrt (foldr (+) 0 ( map (flip (^) 2) datos))

-- Ejercicio 7
-- Implementar la función extraerFeatures :: [Extractor] -> [Texto] -> Datos que permita aplicar 
-- varios extractores a todos los programas que se reciban como parámetro y de esta manera lograr
-- obtener una matriz de atributos. Para ello, primero deberán normalizar utilizando los mismos
-- programas pasados como parámetros, todos los extractores antes de ser aplicados.

extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures = \extractores texts -> map (\extractor -> map (\text -> normalizarExtractor texts extractor text) texts ) extractores

distEuclideana :: Medida
distEuclideana = \v1 v2 -> sqrt (foldr (+) 0  (map (\t -> ((fst t) - (snd t)) ^ 2) (zip v1 v2)))

distCoseno :: Medida
distCoseno = \v1 v2 -> (foldr (+) 0 ( map (\t ->((fst t) * (snd t))) (zip v1 v2) ) ) / ( (norma2 v1) * (norma2 v2) )

-- Ejercicio 9
-- El algoritmo de clasificación que implementaremos es un modelo simple:
-- K-Vecinos Más Cercanos (https://es.wikipedia.org/wiki/K-vecinos_m%C3%A1s_cercanos).
-- La versión que utilizaremos del modelo funciona como se explica a continuación:
--    - Ante una instancia que debe etiquetar, se calcula la distancia a todas las instancias de
--      entrenamiento utilizando alguna medida de distancia definida previamente.
--    - Una vez computadas las distancias, se seleccionan las K instancias más cercanos y se obtiene
--      su etiqueta.
--    - Ante esta lista de etiquetas, se calcula la moda estadı́stica.
--    - Luego, se etiqueta a la nueva instancia con esa moda
--
-- Implementar un clasificador de K-Vecinos más cercanos. knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo .
-- El primer parámetro corresponde al número de vecinos K, luego recibe datos de
-- entrenamiento junto a sus etiquetas y una medida de distancia, devuelve un modelo de predicción.

knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn k datos etiquetas medida = \instancia -> let distancias = map (medida instancia) datos in
                                 moda (kmenores k (zip distancias etiquetas))

moda :: [(Float,Etiqueta)] -> Etiqueta
moda xs = snd (maximum (cuentas (snd (unzip xs))))

kmenores :: Int -> [(Float,Etiqueta)] -> [(Float,Etiqueta)]
kmenores k datos = take k (sort datos)

-- Ejercicio 10
-- Como primer paso, necesitamos separar nuestros datos en datos de entrenamiento y datos de
-- validación, para ello utilizaremos la siguiente función:
-- separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
-- Esta función, toma una matriz de datos xs y sus respectivas etiquetas y, luego dos números n y p,
-- y devuelve el resultado de partir la matriz xs en n particiones dejando la partición número p
-- para validación y el resto para entrenamiento. Es precondición que el número de partición estará
-- entre 1 y n. Para este ejercicio, se debe mantener el orden original en estas particiones.

-- TODO Hacer mas legibles estas funciones. Es inentendible
separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos datos etiquetas n p = let particiones = (particionar (dropLastElements (zip datos etiquetas) n ) n) in
                                       (xTrain particiones (p-1) , yTrain particiones (p-1), xVal particiones (p-1), yVal particiones (p-1))

particionar :: [(Instancia,Etiqueta)] -> Int -> [[(Instancia,Etiqueta)]]
particionar ls n = let tamano = (div (length ls) n ) in
                        foldr (\x r -> if (length (head r) == tamano) then [[x]] ++ r  else (([x]++(head r)):(tail r)) ) [[]] ls

xTrain :: [[(Instancia,Etiqueta)]] -> Int -> Datos
xTrain ls p  = fst (unzip (concat (eliminarElemento ls p)))

xVal :: [[(Instancia,Etiqueta)]] -> Int -> [Etiqueta]
xVal ls p = snd (unzip (concat (eliminarElemento ls p)))

eliminarElemento :: [a] -> Int -> [a]
eliminarElemento ls i = (take i ls) ++ (drop (i+1) ls)

yTrain :: [[(Instancia,Etiqueta)]] -> Int -> Datos
yTrain ls p = fst (unzip (ls !! p))

yVal :: [[(Instancia,Etiqueta)]] -> Int -> [Etiqueta]
yVal ls p = snd (unzip (ls !! p))

dropLastElements :: [a] -> Int -> [a]
dropLastElements ls n = take (length ls - (mod (length ls) n)) ls

---------------------------------------------------
accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy = undefined

nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation = undefined
