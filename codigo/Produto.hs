--produto tem id, nome, preço, sintomas e data de validade
--atualizar preço de um produto permitindo que descontos sejam aplicados
--visualizar lista de produtos (produtos existentes no estoque e produtos de acordo com os sintomas)

module Produto (getProdutoPeloId, getIdProduto, fromIO, converteSintomasEmLista, getProdutosEmLista,
    getSintomasProduto,
    getSintomasProdutoToString,
    produtoToString,
    getSintomasProdutos,
    getProdutos,
    formataParaEscrita,
    escreverArquivo,
    converteEmLista,
    setPreco,
    Produtos(Produtos),
    Produto(Produto)
) where
import System.IO
import System.Directory
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



getProdutoPeloId :: Int -> [Produto] -> Maybe Produto
getProdutoPeloId id [] = Nothing
getProdutoPeloId id (p:ps) = if id == getIdProduto p then Just p
    else getProdutoPeloId id ps

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



escreverArquivo :: [Produto] -> IO ()
escreverArquivo produto = do
    arq <- openFile "../arquivos/Produtos.csv" AppendMode
    arq1 <- openFile "../arquivos/SintomasProduto.csv" AppendMode
    
    print (produto) -- mudar isso, mas fazer depois

    let dataSintomasProduto = getSintomasProdutos produto
    hPutStr arq1 (dataSintomasProduto)
    hPutStr arq (formataParaEscrita produto)
    hClose arq1
    hClose arq


formataParaEscrita :: [Produto] -> String
formataParaEscrita [] = []
formataParaEscrita (c:cs) = produtoToString c ++ "\n" ++ formataParaEscrita cs


------------------------- Visualização de Produtos -------------------------
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

-- Converte IO em puro
fromIO :: IO[String] -> [String]
fromIO x = (unsafePerformIO x :: [String])
------------------------- Visualização de Sintomas -------------------------
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
---------------------------------------------------------------------------