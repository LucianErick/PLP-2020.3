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


    