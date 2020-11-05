import Produto
import System.IO
import Util


data Cliente = Cliente{
    nomeCliente :: String,
    cpf :: String,
    data_cadastro :: String,
    compras :: [Produto],
    sintomas :: [String]
} deriving (Show, Read)


data Clientes = Clientes{
    clientes :: [(String, Cliente)]
} deriving Show

---------------------Cliente-----------------------------------


getAtributosCliente :: Cliente -> String
getAtributosCliente (Cliente {nome = n, cpf = c, data_cadastro = d, compras = com, sintomas = s}) = n++","++c ++","++d ++","++ show(produtosToString com) ++ "," ++ show s



getId :: Produto -> Int
getId (Produto {idProduto = i}) = i


getClientes :: Clientes -> [Cliente]
getClientes (Clientes {clientes = c}) = getClientesFromTuple c


getClientesFromTuple :: [(String, Cliente)] -> [Cliente]
getClientesFromTuple [] = []
getClientesFromTuple ((_,c): cs) = c : getClientesFromTuple cs


getCpf :: Cliente -> String
getCpf Cliente {cpf = c} = c


mapeiaCpf :: [Cliente] -> [(String, Cliente)]
mapeiaCpf [] = []
mapeiaCpf (c:cs)= (getCpf c, c) : mapeiaCpf cs


adicionaProdutoCliente :: [Cliente] -> String -> Produto -> Maybe [Cliente]
adicionaProdutoCliente [] cpfCliente novoProduto = Nothing
adicionaProdutoCliente (Cliente {nome = n, cpf= c, data_cadastro =d, compras =comp, sintomas=s}:cs) cpfCliente novoProduto
    | c == cpfCliente = Just ([Cliente n c d (comp++[novoProduto]) s] ++ cs)
    | otherwise = adicionaProdutoCliente cs cpfCliente novoProduto

---------------IO arquivo--------------------

escreverArquivo :: Clientes -> IO ()
escreverArquivo clientes = do
    arq <- openFile "../arquivos/Clientes.csv" WriteMode
    let dataClientes = getClientes clientes
    print "data"
    print dataClientes
    hPutStrLn arq (formataParaEscrita dataClientes)
    hClose arq


--------util-------
formataParaEscrita :: [Cliente] -> String
formataParaEscrita [] = []
formataParaEscrita (c:cs) = getAtributosCliente c ++ "\n" ++ formataParaEscrita cs

----------------------------


main :: IO()
main = do
    let prod = Produto 0 "dipirona" 3.50 ["Dor de cabeca"] "20/12"
    let prod1 = Produto 1 "dorflex" 4.50 ["Dor muscular"] "20/12"
    let prod2 = Produto 2 "rivotril" 15.0 ["ansiedade"] "25/12"
    let cli = Cliente "Edu" "149" "31/10" [prod, prod1] []
    print "Lendo"
    --contents <- readFile "../arquivos/Clientes.csv"

    let clis = Clientes[(getCpf cli, cli)]
    let novaLista = getClientes clis ++ [cli]
    let clis1 = Clientes (mapeiaCpf novaLista)
    --print clis1
    escreverArquivo clis1

    let att = adicionaProdutoCliente (getClientes clis1) "149" prod2
    let clis2 = Clientes (mapeiaCpf (removeJustMaybeList att))
    escreverArquivo clis2

    --print (split contents '\n')
    --print (length (split contents '\n'))
