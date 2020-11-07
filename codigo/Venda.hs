import System.IO
import Util

data Venda = Venda {
    idVenda :: String,
    cpfFuncionario :: String,
    cpfCliente :: String,
    dataVendas :: String,
    produtos :: [String]
} deriving (Show, Read)


data Vendas = Vendas{
    vendas :: [(String, Venda)]
} deriving Show

-- Getters
getIdVenda :: Venda -> String
getIdVenda (Venda {idVenda = i}) = i

getFuncionarioVenda :: Venda -> String
getFuncionarioVenda (Venda {cpfFuncionario = f}) = f

getClienteVenda :: Venda -> String
getClienteVenda (Venda {cpfCliente = c}) = c

getDataVenda :: Venda -> String
getDataVenda (Venda {dataVendas = d}) = d

-- fazer o retorno em lista de produtos talvez, acho q melhor n
getProdutosVenda :: Venda -> [String]
getProdutosVenda (Venda {produtos = p}) = p

getProdutosVendas :: [Venda] -> String
getProdutosVendas [] = []
getProdutosVendas (venda:vendas) = (getFuncionarioVenda venda) ++ "," ++ getProdutosVendaToString (getProdutosVenda venda) ++ getProdutosVendas vendas

getProdutosVendaToString :: [String] -> String
getProdutosVendaToString [] = []
getProdutosVendaToString (produto:produtos) = if length produtos > 0 then produto ++ "," ++ getProdutosVendaToString produtos else produto ++ "\n"

-- Get vendas retornando uma lista de Venda
getVendas :: Vendas -> [Venda]
getVendas (Vendas {vendas = p}) = getVendasFromTuple p

getVendasFromTuple :: [(String, Venda)] -> [Venda]
getVendasFromTuple [] = []
getVendasFromTuple ((_,c): cs) = c : getVendasFromTuple cs
-----------------------

-- Salvando o arquivo
escreverArquivo :: Vendas -> IO ()
escreverArquivo vendas = do
    arq <- openFile "../arquivos/Vendas.csv" WriteMode
    arq1 <- openFile "../arquivos/ProdutosVendas.csv" WriteMode
    let listaVendas = getVendas vendas
    let listaProdutos = getProdutosVendas listaVendas
    print "data"
    print listaVendas
    hPutStr arq (formataParaEscrita listaVendas)
    hPutStr arq1 (listaProdutos)
    hClose arq
    hClose arq1

-- Parse pra String
formataParaEscrita :: [Venda] -> String
formataParaEscrita [] = []
formataParaEscrita (c:cs) = vendaToString c ++ "\n" ++ formataParaEscrita cs

vendaToString :: Venda -> String
vendaToString Venda {idVenda = id, cpfFuncionario = cpfF, cpfCliente = cpfC, dataVendas = d} = id ++ "," ++ cpfF ++ "," ++ cpfC ++ "," ++ d
-----------------------

-- testes
main :: IO()
main = do
    let v1 = Venda "1" "f127" "c123" "09/09" ["1","4","5"]
    let v2 = Venda "2" "f128" "c122" "10/10" ["1","4","5"]
    let v3 = Venda "3" "f127" "c121" "11/11" ["1","4","5"]
    let vtuple = Vendas [(getIdVenda v1, v1), (getIdVenda v2, v2), (getIdVenda v3, v3)]
    escreverArquivo vtuple
