module Cliente (
    Cliente(Cliente),
    Clientes(Clientes),
    escreverArquivoCliente,
    getClientesEmLista,
    quebraCliente,
    formataExibicaoCliente,
    escreverComprasCliente
) where

import Produto(getProdutoPeloId, getIdProduto, fromIO, converteSintomasEmLista, getProdutosEmLista,
    getSintomasProduto,
    getSintomasProdutoToString,
    produtoToString,
    getSintomasProdutos,
    getProdutos,
    formataParaEscrita,
    Produtos,
    Produto(Produto))
import System.IO
import Util
import System.IO.Unsafe


data Cliente = Cliente{
    nomeCliente :: String,
    cpf :: String,
    data_cadastro :: String,
    compras :: [Produto],
    sintomasCliente :: [String]
} deriving (Show, Read)


data Clientes = Clientes{
    clientes :: [(String, Cliente)]
} deriving Show

---------------------Cliente Getters-----------------------------------


getAtributosCliente :: Cliente -> String
getAtributosCliente (Cliente {nomeCliente = n, cpf = c, data_cadastro = d}) = n++","++c ++","++ d


getClientes :: Clientes -> [Cliente]
getClientes (Clientes {clientes = c}) = getClientesFromTuple c


getSintomasCliente :: Cliente -> [String]
getSintomasCliente Cliente {sintomasCliente = c} = c

getClientesFromTuple :: [(String, Cliente)] -> [Cliente]
getClientesFromTuple [] = []
getClientesFromTuple ((_,c): cs) = c : getClientesFromTuple cs

getComprasCliente :: Cliente -> [Produto]
getComprasCliente Cliente {compras = c} = c

getCpf :: Cliente -> String
getCpf Cliente {cpf = c} = c

--------------

mapeiaCpf :: [Cliente] -> [(String, Cliente)]
mapeiaCpf [] = []
mapeiaCpf (c:cs)= (getCpf c, c) : mapeiaCpf cs


adicionaProdutoCliente :: [Cliente] -> String -> Produto -> Maybe [Cliente]
adicionaProdutoCliente [] cpfCliente novoProduto = Nothing
adicionaProdutoCliente (Cliente {nomeCliente = n, cpf= c, data_cadastro =d, compras =comp, sintomasCliente=s}:cs) cpfCliente novoProduto
    | c == cpfCliente = Just ([Cliente n c d (comp++[novoProduto]) s] ++ cs)
    | otherwise = adicionaProdutoCliente cs cpfCliente novoProduto


getSintomasClientesToCsv :: [Cliente] -> String
getSintomasClientesToCsv [] = []
getSintomasClientesToCsv (c:cs) = if length sintomasCli > 0 then getCpf c ++ "," ++ getSintomasClienteToString (sintomasCli) ++ getSintomasClientesToCsv cs
    else []
    where
        sintomasCli = getSintomasCliente c


getSintomasClienteToString :: [String] -> String
getSintomasClienteToString [] = []
getSintomasClienteToString (c:cs) = if length cs > 0 then c ++ "," ++ getSintomasClienteToString cs else c ++ "\n"

getComprasClientesToCsv :: [Cliente] -> String
getComprasClientesToCsv [] = []
getComprasClientesToCsv (c:cs) = if length comprasCli > 0 then getCpf c ++ "," ++ getComprasClienteToString (comprasCli) ++ getComprasClientesToCsv cs
    else []
    where
        comprasCli = getComprasCliente c

getComprasClienteToString :: [Produto] -> String
getComprasClienteToString [] = []

---------------IO arquivo--------------------


escreverArquivoCliente :: [Cliente] -> IO ()
escreverArquivoCliente clientes = do
    arq <- openFile "../arquivos/Clientes.csv" AppendMode
    arqSintomasClientes <- openFile "../arquivos/SintomasClientes.csv" AppendMode
    let dataSintomasCliente = getSintomasClientesToCsv clientes
    let comprasToCsv = iteraComprasClientes clientes
    let sintomasToCsv = iteraSintomasProdutosClientes clientes
    putStrLn comprasToCsv
    putStrLn sintomasToCsv 
    hPutStr arq (formataParaEscritaClientes clientes)
    hPutStr arqSintomasClientes dataSintomasCliente
    escreverComprasCliente comprasToCsv sintomasToCsv
    hClose arq
    hClose arqSintomasClientes

iteraComprasClientes :: [Cliente] -> String
iteraComprasClientes [] = []
iteraComprasClientes (c:cs) = iteraFormatoProdutos cpfAtual prodsAtuais ++ iteraComprasClientes cs
    where
        prodsAtuais = getComprasCliente c
        cpfAtual = getCpf c

iteraFormatoProdutos :: String -> [Produto] -> String
iteraFormatoProdutos cpf [] = []
iteraFormatoProdutos cpf (p:ps) = cpf ++ "," ++ show(getIdProduto p) ++ "\n" ++ iteraFormatoProdutos cpf ps

iteraSintomasProdutosClientes :: [Cliente] -> String
iteraSintomasProdutosClientes [] = []
iteraSintomasProdutosClientes (c:cs) = iteraFormatoSintomas cpfAtual prodsAtuais ++ iteraSintomasProdutosClientes cs
    where
        prodsAtuais = getComprasCliente c
        cpfAtual = getCpf c

iteraFormatoSintomas :: String -> [Produto] -> String
iteraFormatoSintomas cpf [] = []
iteraFormatoSintomas cpf (s:ss) = cpf ++ "," ++ getSintomasProdutoToString sintomas ++ iteraFormatoSintomas cpf ss
    where
        sintomas = getSintomasProduto s

escreverComprasCliente :: String -> String -> IO ()
escreverComprasCliente produtos sintomas = do
    arq <- openFile "../arquivos/ComprasClientes.csv" AppendMode
    arq1 <- openFile "../arquivos/SintomasProdutosClientes.csv" AppendMode

    hPutStr arq (produtos)
    hPutStr arq1 (sintomas)
    hClose arq1
    hClose arq

--------------- IO Leitura -------------

getClientesEmLista :: IO [Cliente]
getClientesEmLista = do
    listaClienteStr <- lerClientes
    let clientes = (converteEmListaCliente listaClienteStr)
    return clientes

converteEmListaCliente :: [String] -> [Cliente]
converteEmListaCliente [] = []
converteEmListaCliente (produto:lista) =
    converteEmCliente (split produto ',') : converteEmListaCliente lista

converteEmCliente :: [String] -> Cliente
converteEmCliente cliente = Cliente nome cpf dataCadastro compras sintomas
    where
        nome = cliente !! 0
        cpf = cliente !! 1
        dataCadastro = cliente !! 2
        sintomas = fromIO(getSintomasClienteEmLista cpf)
        compras = removeJustListOfMaybe(unsafePerformIO(filtraProdutosCliente cpf))

filtraProdutosCliente :: String -> IO [Maybe Produto]
filtraProdutosCliente  cpf = do
    produtos <- getProdutosEmLista
    listaProdutoClientesStr <- lerProdutosClientes
    let listaProdutos = converteComprasEmLista listaProdutoClientesStr
    return $ (varreListaProdutos cpf produtos listaProdutos)

varreListaProdutos :: String ->[Produto] ->[[String]] -> [Maybe Produto]
varreListaProdutos cpf produtos [] = []
varreListaProdutos cpf produtos [[]] = []
varreListaProdutos cpf produtos (prodClientedb:lista1)
    | cpf == head prodClientedb = getProdutoPeloId idProduto produtos : varreListaProdutos cpf produtos lista1
    | otherwise = varreListaProdutos cpf produtos lista1
    where
        idProduto = (read (prodClientedb !! 1)::Int)

getSintomasClienteEmLista :: String -> IO [String]
getSintomasClienteEmLista cpf = do
    sintomas <- openFile "../arquivos/SintomasClientes.csv" ReadMode
    listaSintomas <- lines <$> hGetContents sintomas
    let sintomasFinal = converteSintomasEmLista listaSintomas
    return $ (filtraSintomaCliente cpf sintomasFinal)

lerClientes :: IO[String]
lerClientes = do
    arq <- openFile "../arquivos/Clientes.csv" ReadMode
    conteudo <- lines <$> hGetContents arq
    return conteudo


lerProdutosClientes :: IO [String]
lerProdutosClientes = do
    arq <- openFile "../arquivos/ComprasClientes.csv" ReadMode
    conteudo <- lines <$> hGetContents arq
    return conteudo

lerSintomasClientes :: IO [String]
lerSintomasClientes = do
    arq <- openFile "../arquivos/SintomasClientes.csv" ReadMode
    conteudo <- lines <$> hGetContents arq
    return  conteudo



--------util-------

filtraSintomaCliente :: String -> [[String]] -> [String]
filtraSintomaCliente cpf [[]] = []
filtraSintomaCliente cpf [] = []
filtraSintomaCliente cpf (s:ss)
    | cpf == head s = tail s
    | otherwise =  filtraSintomaCliente cpf ss

filtraIdProdutoCliente :: String -> [[String]] -> [String]
filtraIdProdutoCliente cpf [[]] = []
filtraIdProdutoCliente cpf [] = []
filtraIdProdutoCliente cpf (s:ss)
    | cpf == head s = tail s
    | otherwise =  filtraSintomaCliente cpf ss

formataParaEscritaClientes :: [Cliente] -> String
formataParaEscritaClientes [] = []
formataParaEscritaClientes (c:cs) = getAtributosCliente c ++ "\n" ++ formataParaEscritaClientes cs

converteComprasEmLista :: [String] -> [[String]]
converteComprasEmLista [] = []
converteComprasEmLista (sintoma:lista) =
    (split sintoma ',') : converteComprasEmLista lista


quebraCliente :: String -> [String]
quebraCliente entrada = split entrada ','

formataExibicaoCliente :: [String] -> String
formataExibicaoCliente lista = "Nome: " ++ (lista !! 0) ++ " | cpf:" ++ (lista !! 1) ++ " | data de cadastro:" ++ (lista !! 2)

----------------------------


main :: IO()
main = do
    -- let prod = Produto 0 "dipirona" 3.50 ["Dor de cabeca"] "20/12"
    -- let prod1 = Produto 1 "dorflex" 4.50 ["Dor muscular"] "20/12"
    -- let prod2 = Produto 2 "rivotril" 15.0 ["ansiedade"] "25/12"
    -- let cli = Cliente "Edu" "149" "31/10" [prod, prod1] ["dor de cabeca"]
    -- let cli1 = Cliente "Edu2" "159" "31/10" [prod, prod1] []

    print (formataExibicaoCliente (quebraCliente "Olá, sou, luciano"))
    -- --contents <- readFile "../arquivos/Clientes.csv"

    -- let clis = Clientes[(getCpf cli, cli)]
    -- let novaLista = getClientes clis ++ [cli1]
    -- let clis1 = Clientes (mapeiaCpf novaLista)
    -- --print clis1
    -- --escreverArquivo clis1

    -- let att = adicionaProdutoCliente (getClientes clis1) "149" prod2
    -- let clis2 = Clientes (mapeiaCpf (removeJustMaybeList att))
    -- escreverArquivoCliente clis2

    -- x <- getClientesEmLista
    -- print x


    --print (split contents '\n')
    --print (length (split contents '\n'))
