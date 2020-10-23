
data Cliente = Cliente{
    nome :: String,
    cpf :: String,
    data_cadastro :: String,
    compras :: [(Int, String, Float, [String], String)],
    sintomas :: [String]
}


data Clientes = Clientes{
    clientes :: [(String, Cliente)]
}
