--produto tem id, nome, preço, sintomas e data de validade
--atualizar preço de um produto permitindo que descontos sejam aplicados
--visualizar lista de produtos (produtos existentes no estoque e produtos de acordo com os sintomas)

module Produto where
import System.IO
import Util
import System.IO.Unsafe

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
    hPutStr arq1 (dataSintomasProduto)
    hPutStr arq (formataParaEscrita dataProdutos)
    hClose arq1
    hClose arq


formataParaEscrita :: [Produto] -> String
formataParaEscrita [] = []
formataParaEscrita (c:cs) = produtoToString c ++ "\n" ++ formataParaEscrita cs



getProdutosEmLista :: IO [Produto]
getProdutosEmLista = do
    produtos <- openFile "../arquivos/Produtos.csv" ReadMode
    listaProdutos <- lines <$> hGetContents produtos
    return $ (converteEmLista listaProdutos)

converteEmLista :: [String] -> [Produto]
converteEmLista [] = []
converteEmLista (produto:lista) =
    converteEmProduto (split produto ',') : converteEmLista lista



converteEmProduto :: [String] -> Produto
converteEmProduto produto = Produto id nome preco sintomasProduto dataValidade
    where 
        id = (read (produto !! 0) :: Int)
        nome = produto !! 1
        preco = (read (produto !! 2) :: Double)
        sintomasProduto = fromIO(getSintomasEmLista id)
        dataValidade = produto !! 3

fromIO :: IO[String] -> [String]
fromIO x = (unsafePerformIO x :: [String])



getSintomasEmLista :: Int -> IO [String]
getSintomasEmLista id = do
    sintomas <- openFile "../arquivos/SintomasProduto.csv" ReadMode
    listaSintomas <- lines <$> hGetContents sintomas
    let sintomasFinal = converteSintomasEmLista listaSintomas
    return $ (filtraSintoma id sintomasFinal)



converteSintomasEmLista :: [String] -> [[String]]
converteSintomasEmLista [] = []
converteSintomasEmLista (sintoma:lista) =
    (split sintoma ',') : converteSintomasEmLista lista
    


filtraSintoma :: Int -> [[String]] -> [String]
filtraSintoma _ [] = []
filtraSintoma id (sintoma:sintomas)
    | id == idCliente = tail sintoma
    | otherwise = filtraSintoma id sintomas
    where
        idCliente = read (head sintoma) :: Int



main :: IO()
main = do
    -- let p1 = Produto 1 "a" 1.0 ["b", "e"] "1/1"
    -- let p2 = Produto 2 "c" 2.0 ["d", "f", "g"] "1/1"
    -- let p3 = [(getIdProduto p1, p1), (getIdProduto p2, p2)]
    -- let p4 = Produtos p3
    -- escreverArquivo p4
    
    produtos <- openFile "../arquivos/Produtos.csv" ReadMode
    listaProdutos <- lines <$> hGetContents produtos
    print listaProdutos
    -- sintomas <- openFile "../arquivos/SintomasProduto.csv" ReadMode
    -- listaSintomas <- lines <$> hGetContents sintomas
    -- print (converteSintomasEmLista listaSintomas)
    -- print (getProdutosEmLista)
    -- print (show getProdutosEmLista)
    -- let produto = split (head listaProdutos) ','
    -- let id = (read (produto !! 0) :: Int)
    -- let nome = produto !! 1
    -- let preco = (read (produto !! 2) :: Double)
    -- let dataValidade = produto !! 3
    -- print (converteEmProduto (split (head listaProdutos) ','))
    -- let x = Produto id nome preco ["pinto mole"] dataValidade
    -- print x

    -- print (read id :: Int)
    -- print nome
    -- print (read preco :: Double )
    -- print dataValidade

