module Lib
( 
    lstvezesx,
    invertelst,
    powlist
) where

-- ================================================
-- ==================== LAB 01 ====================
-- ================================================

lstvezesx [] x = []
lstvezesx (h:t) x = h*x : (lstvezesx t x)

invertelst [] = []
invertelst (h:t) = invertelst t ++ [h]

pow :: Int -> Int -> Int 
pow x 0 = 1
pow x 1 = x
pow x p = x * (pow x (p-1))

powlist [] x = []
powlist (h:t) x = (pow h x) : (powlist t x)

-- ================================================
-- ==================== LAB 02 ====================
-- ================================================

-- ===== Q1 =====
pertenceLst :: Float -> [Float] -> Bool
pertenceLst x [] = False
pertenceLst x (h:t) | h == x = True
                    | h /= x = (pertenceLst x t)

-- ===== Q2 =====
ocorrencias :: Float -> [Float] -> Float
ocorrencias x [] = 0
ocorrencias x (h:t) | h == x = 1 + (ocorrencias x t)
                    | h /= x = 0 + (ocorrencias x t)

-- ===== Q3 =====
repeticoes :: Float -> Float -> [Float]
repeticoes e n | n == 0 = []
               | otherwise = e : (repeticoes e (n - 1))

-- ===== Q4 =====
verificaPositivo :: Int -> Bool
verificaPositivo n | n < 0 = False
                   | otherwise = True

positivos :: [Int] -> [Bool]
positivos [] = []
positivos lista = map verificaPositivo lista

-- ===== Q5 =====
normaVetor :: [Float] -> Float
normaVetor = (\lista -> case lista of
    [] -> 0
    lista -> (sqrt (foldl (+) 0 (map (\x -> x * x) lista))))

-- ===== Q6 =====
criaPares :: [Float] -> [Float] -> [(Float,Float)]
criaPares (x:xs) (y:ys) = (x, y) : (criaPares xs ys)
criaPares [] (y:ys) = (0, y) : (criaPares [] ys)
criaPares (x:xs) [] = (x, 0) : (criaPares xs [])
criaPares [] [] = []

multiplicacaoProdutoInterno :: (Float,Float) -> Float
multiplicacaoProdutoInterno (x, y) = x * y

produtoInternoVetores :: [Float] -> [Float] -> Float
produtoInternoVetores (listaX) (listaY) = if (listaX == []) && (listaY == []) then 0 else foldl (+) 0 (map multiplicacaoProdutoInterno (criaPares (listaX) (listaY)))

-- ===== Q7 =====
similaridadeDocumentos :: [Float] -> [Float] -> Float
similaridadeDocumentos (listaX) (listaY) = if (listaX == []) && (listaY == []) then 0 else ((produtoInternoVetores (listaX) (listaY)) / ((normaVetor (listaX)) * (normaVetor (listaY))))

-- ===== Q8 =====
distanciaDrManhattan :: [Float] -> [Float] -> Float
distanciaDrManhattan = (\listaX -> (\listaY -> 
    foldl (+) 0 (
        if (listaX == []) || (listaY == []) 
            then 
                [0] 
            else 
                (map (\(z, w) -> if z - w < 0 then (z - w) * (-1) else (z-w)) (criaPares listaX listaY)))))

-- ================================================
-- ==================== LAB 03 ====================
-- ================================================

-- ===== Q1 =====
potencias :: Int -> [Int]
potencias n = [x*x | x <- [1..n]]

-- ===== Q2 =====
listaElevadoNaTres :: Int -> [Int]
listaElevadoNaTres n = take n listaElevadoNaTres where listaElevadoNaTres = 2:map (\x -> pow x 3) listaElevadoNaTres

-- ===== Q3 =====
auxFibonnaci :: Int -> Int -> [Int]
auxFibonnaci 0 0 = []
auxFibonnaci m n = m + n : (auxFibonnaci n (m + n))

calculoFibonnaci :: Int -> [Int]
calculoFibonnaci n = take n calculoFibonnaci  where calculoFibonnaci  = 0:1:auxFibonnaci 0 1

