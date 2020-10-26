module Produto
where


import qualified Data.Map as Map

data Produto = Produto{
    idProduto :: Int,
    nomeProduto :: String,
    preco :: Float,
    sintomasProduto :: [String],
    validade :: String
} deriving (Show)


data Produtos = Produtos{
    produtos :: [(Int,Produto)]
}


getProdutos :: Produtos -> [Produto]
getProdutos (Produtos {produtos = p}) = Map.elems $ (Map.fromList p)


getIdProduto :: Produto -> Int
getIdProduto (Produto {idProduto = k}) = k

getNomeProduto :: Produto -> String
getNomeProduto (Produto {nomeProduto = n}) = n

getSintomasProduto :: Produto -> [String]
getSintomasProduto (Produto {sintomasProduto = s}) = s



getProdutoPeloNome :: String -> Produtos -> Maybe Produto
getProdutoPeloNome " " (Produtos p) = Nothing
getProdutoPeloNome nome (Produtos p) = varrePeloNome nome produtos
    where
        produtos = getProdutos (Produtos p)


varrePeloNome :: String -> [Produto] -> Maybe Produto
varrePeloNome nome [] = Nothing
varrePeloNome nome (p:ps)
    | nome == getNomeProduto p = Just p
    | otherwise = varrePeloNome nome ps



getProdutosPorSintoma :: String -> Produtos -> [Maybe Produto]
getProdutosPorSintoma " " (Produtos p) = [Nothing]
getProdutosPorSintoma sintoma (Produtos p) = varrePorSintoma sintoma produtos
    where
        produtos = getProdutos (Produtos p)

varrePorSintoma :: String -> [Produto] -> [Maybe Produto]
varrePorSintoma sintoma [] = []
varrePorSintoma sintoma (p:ps)
    | elem sintoma (getSintomasProduto p) = [Just p] ++ varrePorSintoma sintoma ps
    | otherwise = varrePorSintoma sintoma ps