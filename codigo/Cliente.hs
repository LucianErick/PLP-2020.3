import Produto


data Cliente = Cliente{
    nomeCliente :: String,
    cpf :: String,
    data_cadastro :: String,
    compras :: [Produto],
    sintomasCliente :: [String]
} deriving (Show)

data Clientes = Clientes{
    clientes :: [(String, Cliente)]
}


getProdutosCliente :: Cliente -> [Produto]
getProdutosCliente (Cliente {compras = c}) = c
