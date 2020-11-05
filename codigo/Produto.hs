--produto tem id, nome, preço, sintomas e data de validade
--atualizar preço de um produto permitindo que descontos sejam aplicados
--visualizar lista de produtos (produtos existentes no estoque e produtos de acordo com os sintomas)

import System.IO

data Produto = Produto {
    idProduto :: Int,
    nomeProduto :: String,
    preco :: Double,
    sintomasProduto :: [String],
    validade :: String
} deriving (Show, Read)



data Produtos = Produtos {
    produtos :: [(Int, Produto)]
} deriving Show



getProdutos :: Produtos -> [Produto]
getProdutos (Produtos {produtos = p}) = getProdutosFromTuple p



getProdutosFromTuple :: [(Int, Produto)] -> [Produto]
getProdutosFromTuple [] = []
getProdutosFromTuple ((_,c): cs) = c : getProdutosFromTuple cs



getIdProduto :: Produto -> Int
getIdProduto Produto {idProduto = i} = i



getNomeProduto :: Produto -> String
getNomeProduto Produto {nomeProduto = n} = n



getPreco :: Produto -> Double
getPreco Produto {preco = p} = p 



getSintomasProduto :: Produto -> [String]
getSintomasProduto Produto {sintomasProduto = s} = s



getSintomasProdutos :: [Produto] -> String
getSintomasProdutos [] = []
getSintomasProdutos (c:cs) = show (getIdProduto c) ++ "," ++ getSintomasProdutoToString (getSintomasProduto c) ++ getSintomasProdutos cs



getSintomasProdutoToString :: [String] -> String
getSintomasProdutoToString [] = []
getSintomasProdutoToString (c:cs) = if length cs > 0 then c ++ "," ++ getSintomasProdutoToString cs else c ++ "\n"



getValidade :: Produto -> String
getValidade Produto {validade = v} = v



setPreco :: [Produto] -> Int -> Double -> Maybe [Produto]
setPreco [] x novoPreco = Nothing
setPreco (c:cs) x novoPreco
    | idAtual == x = Just ([Produto x nomeProdutoAtual novoPreco sintomasProdutoAtual validadeAtual] ++ cs)
    | otherwise = setPreco cs x novoPreco
    where
        idAtual = getIdProduto c
        nomeProdutoAtual = getNomeProduto c
        sintomasProdutoAtual = getSintomasProduto c
        validadeAtual = getValidade c



produtoToString :: Produto -> String
produtoToString Produto {idProduto = i, nomeProduto = n, preco = p, validade = v} = show i ++"," ++ n ++ "," ++ show p ++ "," ++ v



produtosToString :: [Produto] -> String
produtosToString [] = []
produtosToString (p:ps) = if length ps > 0 then do "["++produtoToString p ++"]," ++ produtosToString ps
    else do "[" ++ produtoToString p ++ "]"



sintomasToString :: [String] -> String
sintomasToString [] = []
sintomasToString (s:sw) = s ++ sintomasToString sw



escreverArquivo :: Produtos -> IO ()
escreverArquivo produtos = do
    arq <- openFile "../arquivos/Produtos.csv" WriteMode
    arq1 <- openFile "../arquivos/SintomasProduto.csv" WriteMode
    let dataProdutos = getProdutos produtos
    let dataSintomasProduto = getSintomasProdutos dataProdutos
    print "data"
    print dataProdutos
    hPutStrLn arq1 (dataSintomasProduto)
    hPutStrLn arq (formataParaEscrita dataProdutos)
    hClose arq1
    hClose arq



formataParaEscrita :: [Produto] -> String
formataParaEscrita [] = []
formataParaEscrita (c:cs) = produtoToString c ++ "\n" ++ formataParaEscrita cs



main :: IO()
main = do
    let p1 = Produto 1 "a" 1.0 ["b", "e"] "1/1"
    let p2 = Produto 2 "c" 2.0 ["d", "f", "g"] "1/1"
    let p3 = [(getIdProduto p1, p1), (getIdProduto p2, p2)]
    let p4 = Produtos p3
    escreverArquivo p4
