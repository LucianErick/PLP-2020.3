module Funcionario (
    Funcionario(Funcionario),
    Funcionarios(Funcionarios),
    escreverArquivoFuncionario

) where

import System.IO
import Util
import System.IO.Unsafe

-- funcionário deve ser cadastrado com a lista de vendas vazias, e então usar métodos 
-- pra adicionar as vendas dele
data Funcionario = Funcionario {
    nomeFuncionario :: String,
    cpfFuncionario :: String,
    dataAdmissao :: String,
    vendas :: [String],
    salario :: Double
} deriving (Show, Read)

data Funcionarios = Funcionarios{
    funcionarios :: [(String, Funcionario)]
} deriving Show
 
--------------------------- Funcionário ------------------------------------

-- já implementado em Venda.hs
-- getId :: Venda -> Int
-- getId (Venda {idVenda = i}) = i

mapeiaFuncionarioPorCpf :: [Funcionario] -> [(String, Funcionario)]
mapeiaFuncionarioPorCpf [] = []
mapeiaFuncionarioPorCpf (f:fLista)= (getCpfFuncionario f, f) : mapeiaFuncionarioPorCpf fLista


getCpfFuncionario :: Funcionario -> String
getCpfFuncionario Funcionario {cpfFuncionario = c} = c

getNomeFuncionario :: Funcionario -> String
getNomeFuncionario Funcionario {nomeFuncionario = n} = n

getDataAdmissao :: Funcionario -> String
getDataAdmissao Funcionario {dataAdmissao = n} = n

getVendas :: Funcionario -> [String]
getVendas Funcionario {vendas = n} = n


--------------------------- Visualização de Funcionário -------------------------------
getFuncionariosPuros :: [Funcionario]
getFuncionariosPuros = (unsafePerformIO getFuncionariosEmLista :: [Funcionario])

-----------------------------VISUALIZACAO-------------------------------

getFuncionariosEmLista :: IO [Funcionario]
getFuncionariosEmLista = do
    funcionarios <- openFile "../arquivos/Funcionarios.csv" ReadMode
    listaFuncionarios <- lines <$> hGetContents funcionarios
    return $ (converteEmLista listaFuncionarios)

converteEmLista :: [String] -> [Funcionario]
converteEmLista [] = []
converteEmLista (funcionario:lista) =
    converteEmFuncionario (split funcionario ',') : converteEmLista lista

converteEmFuncionario :: [String] -> Funcionario
converteEmFuncionario funcionario = Funcionario nome cpfFuncionario dataAdmissao vendas salario
    where 
        nome = funcionario !! 0
        cpfFuncionario = funcionario !! 1
        dataAdmissao = funcionario !! 2
        vendas = fromIO(getVendasEmLista cpfFuncionario)
        salario = (read (funcionario !! 3) :: Double)

-- Converte IO em puro
fromIO :: IO[String] -> [String]
fromIO x = (unsafePerformIO x :: [String])

---------------- Visualização de Vendas ----------------
getVendasEmLista :: String -> IO [String]
getVendasEmLista cpf = do
    vendas <- openFile "../arquivos/Vendas.csv" ReadMode
    listaVendas <- lines <$> hGetContents vendas
    let vendasFinal = converteVendasEmLista listaVendas
    return $ (filtraVenda cpf vendasFinal)

converteVendasEmLista :: [String] -> [[String]]
converteVendasEmLista [] = []
converteVendasEmLista (venda:vendas) =
    (split venda ',') : converteVendasEmLista vendas

filtraVenda :: String -> [[String]] -> [String]
filtraVenda _ [] = []
filtraVenda cpf (venda:vendas)
    | cpf == cpfFuncionario = (head venda) : filtraVenda cpf vendas
    | otherwise = filtraVenda cpf vendas
    where
        cpfFuncionario = venda !! 1

-------------------------------UTIL----------------------------------------

excluiFuncionario :: [Funcionario] -> String -> [Funcionario] -> [Funcionario] 
excluiFuncionario [] cpf [] = []
excluiFuncionario (funcionario:funcionarios) cpf aux
    | getCpfFuncionario funcionario == cpf = aux ++ funcionarios
    | otherwise = excluiFuncionario funcionarios cpf (aux ++ [funcionario])

getFuncionarioPeloCpf :: String -> [Funcionario] -> Maybe Funcionario
getFuncionarioPeloCpf cpf [] = Nothing
getFuncionarioPeloCpf cpf (p:ps) = if cpf == getCpfFuncionario p then Just p
    else getFuncionarioPeloCpf cpf ps

getAtributosFuncionario :: Funcionario -> String
getAtributosFuncionario (Funcionario {nomeFuncionario = n, cpfFuncionario = c, dataAdmissao = d, vendas = v, salario = s}) = n++ "," ++ c ++ "," ++ d ++ "," ++ show s

setSalario :: Funcionario -> Double -> Funcionario
setSalario (Funcionario {nomeFuncionario = nome, cpfFuncionario = cpf, dataAdmissao = d, vendas = v, salario = s}) salarioNovo
    = Funcionario nome cpf d v salarioNovo

adicionaVendaDeFuncionario :: [Funcionario] -> String -> String -> [Funcionario]-> [Funcionario]
adicionaVendaDeFuncionario [] x y aux= []
adicionaVendaDeFuncionario (Funcionario {nomeFuncionario = n, cpfFuncionario = c, dataAdmissao = d, vendas = v, salario = s}:funcionarios) cpf idVenda aux
    | cpf == c = aux ++ [Funcionario n c d (v++[idVenda]) s]
    | otherwise = adicionaVendaDeFuncionario funcionarios cpf idVenda (aux ++ [Funcionario n c d v s])

---------------------------IO FUNCIONARIO-----------------------------------
escreverArquivoFuncionario :: [Funcionario] -> IO ()
escreverArquivoFuncionario funcionarios = do
    arq <- openFile "../arquivos/Funcionarios.csv" AppendMode
    print funcionarios
    hPutStr arq (formataParaEscrita funcionarios)
    hClose arq

getFuncionarios :: Funcionarios -> [Funcionario]
getFuncionarios (Funcionarios {funcionarios = f}) = getFuncionariosFromTuple f

getFuncionariosFromTuple :: [(String, Funcionario)] -> [Funcionario]
getFuncionariosFromTuple [] = []
getFuncionariosFromTuple ((_,f): cs) = f : getFuncionariosFromTuple cs

formataParaEscrita :: [Funcionario] -> String
formataParaEscrita [] = []
formataParaEscrita (f:cs) = getAtributosFuncionario f ++ "\n" ++ formataParaEscrita cs
--------------------------------------

-- testes
-- main :: IO()
-- main = do
--     let f1 = Funcionario "leo" "f127" "09/09" [] 777.0
--     let f2 = Funcionario "eduardo" "f321" "10/10" [] 888.0
--     let f3 = Funcionario "biden" "f22" "07/11" [] 1.0
--     let f4 = Funcionarios (mapeiaFuncionarioPorCpf [f1,f2,f3])
--     escreverArquivoFuncionario f4
--     print "^^^^ funcionarios cadastrados ^^^^"
--     let funcionarios = getFuncionariosPuros
--     print funcionarios
--     print "^^^^ funcionarios resgatados do arquivo ^^^^"

--     -- vendas <- openFile "../arquivos/Vendas.csv" ReadMode
--     -- listaVendas <- lines <$> hGetContents vendas
--     -- let vendasFinal = converteVendasEmLista listaVendas
--     -- let vendasFiltradas = filtraVenda "f127" vendasFinal
--     -- print vendasFiltradas
--     -- funcionarios <- openFile "../arquivos/Funcionarios.csv" ReadMode
--     -- listaFuncionarios <- lines <$> hGetContents funcionarios
--     -- print listaFuncionarios