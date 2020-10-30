--produto tem id, nome, preço, sintomas e data de validade
--atualizar preço de um produto permitindo que descontos sejam aplicados
--visualizar lista de produtos (produtos existentes no estoque e produtos de acordo com os sintomas)

data Produto = Produto {
    id :: Int,
    nome :: String,
    preco :: Double,
    sintomas :: [String],
    validade :: String
}

data Produtos = Produtos {
    produtos :: [(Int, Produto)]
}