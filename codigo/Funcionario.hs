import System.IO
import Util
import System.IO.Unsafe

data Funcionario = Funcionario {
    nomeFuncionario :: String,
    cpfFuncionario :: String,
    data_admissao :: String,
    vendas :: [String],
    salario :: Double
} deriving (Show, Read)


data Funcionarios = Funcionarios{
    funcionarios :: [(String, Funcionario)]
} deriving Show
 
---------------------------FUNCIONARIO------------------------------------


-- já implementado em Venda.hs
-- getId :: Venda -> Int
-- getId (Venda {idVenda = i}) = i

getCpfFuncionario :: Funcionario -> String
getCpfFuncionario Funcionario {cpfFuncionario = c} = c

mapeiaCpf :: [Funcionario] -> [(String, Funcionario)]
mapeiaCpf [] = []
mapeiaCpf (f:cs)= (getCpfFuncionario f, f) : mapeiaCpf cs


getNomeFuncionario :: Funcionario -> String
getNomeFuncionario Funcionario {nomeFuncionario = n} = n

getDataAdmissao :: Funcionario -> String
getDataAdmissao Funcionario {data_admissao = n} = n

getVendas :: Funcionario -> [String]
getVendas Funcionario {vendas = n} = n

setSalario :: Funcionario -> Double -> Funcionario
setSalario (Funcionario {nomeFuncionario = nome, cpfFuncionario = cpf, data_admissao = d, vendas = v, salario = s}) salarioNovo
    = Funcionario nome cpf d v salarioNovo

------- adicional, n precisa excluir funcionario
-- excluiFuncionario :: Funcionarios -> Integer -> Integer -> Funcionarios 
-- excluiFuncionario [] _ _ = []
-- excluiFuncionario (o:os) cursor contador
--    | cursor == contador = excluiFuncionario os cursor (contador+1)
--    | otherwise = o:excluiFuncionario os cursor (contador+1)

-----------------------------VISUALIZACAO-------------------------------
converteFuncionariosPuros :: IO [Funcionario] -> [Funcionario]
converteFuncionariosPuros x = (unsafePerformIO x :: [Funcionario])

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

---------------- visualização vendas ----------------
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

formataParaEscrita :: [Funcionario] -> String
formataParaEscrita [] = []
formataParaEscrita (f:cs) = getAtributosFuncionario f ++ "\n" ++ formataParaEscrita cs

getAtributosFuncionario :: Funcionario -> String
getAtributosFuncionario (Funcionario {nomeFuncionario = n, cpfFuncionario = c, data_admissao = d, vendas = v, salario = s}) = n++ "," ++ c ++ "," ++ d ++ "," ++ show s

getFuncionarioPeloCpf :: String -> [Funcionario] -> Maybe Funcionario
getFuncionarioPeloCpf cpf [] = Nothing
getFuncionarioPeloCpf cpf (p:ps) = if cpf == getCpfFuncionario p then Just p
    else getFuncionarioPeloCpf cpf ps

adicionaVendaDeFuncionario :: [Funcionario] -> String -> String-> [Funcionario]
adicionaVendaDeFuncionario [] x y = []
adicionaVendaDeFuncionario (Funcionario {nomeFuncionario = n, cpfFuncionario = c, data_admissao = d, vendas = v, salario = s}:funcionarios) cpf idVenda
    | cpf == c = [Funcionario n c d (v++[idVenda]) s] ++ funcionarios
    | otherwise = adicionaVendaDeFuncionario funcionarios cpf idVenda
---------------------------IO FUNCIONARIO-----------------------------------

escreverArquivo :: Funcionarios -> IO ()
escreverArquivo funcionarios = do
    arq <- openFile "../arquivos/Funcionarios.csv" WriteMode
    let dataFuncionarios = getFuncionarios funcionarios
    print "data"
    print dataFuncionarios
    hPutStr arq (formataParaEscrita dataFuncionarios)
    hClose arq

getFuncionarios :: Funcionarios -> [Funcionario]
getFuncionarios (Funcionarios {funcionarios = f}) = getFuncionariosFromTuple f


getFuncionariosFromTuple :: [(String, Funcionario)] -> [Funcionario]
getFuncionariosFromTuple [] = []
getFuncionariosFromTuple ((_,f): cs) = f : getFuncionariosFromTuple cs
--------------------------------------

main :: IO()
main = do
    let f1 = Funcionario "leo" "f127" "09/09" ["3","5"] 777.0
    let f2 = Funcionario "eduardo" "f321" "10/10" ["2","8"] 888.0
    let f3 = Funcionario "biden" "f22" "07/11" ["1","2","3"] 1.0
    let f4 = Funcionarios [(getCpfFuncionario f1, f1), (getCpfFuncionario f2, f2), (getCpfFuncionario f3, f3)]
    escreverArquivo f4
    print "^^^funcionarios"
    -- vendas <- openFile "../arquivos/Vendas.csv" ReadMode
    -- listaVendas <- lines <$> hGetContents vendas
    -- let vendasFinal = converteVendasEmLista listaVendas
    -- let vendasFiltradas = filtraVenda "f127" vendasFinal
    -- print vendasFiltradas
    -- funcionarios <- openFile "../arquivos/Funcionarios.csv" ReadMode
    -- listaFuncionarios <- lines <$> hGetContents funcionarios
    -- print listaFuncionarios