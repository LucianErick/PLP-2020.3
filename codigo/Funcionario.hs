import System.IO
import Util
import Venda


data Funcionario = Funcionario {
    nomeFuncionario :: String,
    cpf :: String,
    data_admissao :: String,
    vendas :: [String],
    salario :: Double
} deriving (Show, Read)


data Funcionarios = Funcionarios{
    funcionarios :: [(String, Funcionario)]
} deriving Show
 
---------------------------FUNCIONARIO------------------------------------

getAtributosFuncionario :: Funcionario -> String
getAtributosFuncionario (Funcionario {nome = n, cpf = c, data_admissao = d, vendas = com, salario = s}) = n++","++c ++","++d ++","++ show(produtosToString com) ++ "," ++ show s

getId :: Venda -> Int
getId (Venda {idVenda = i}) = i

getCpf :: Funcionario -> String
getCpf Funcionario {cpf = c} = c

getFuncionarios :: Funcionarios -> [Funcionario]
getFuncionarios (Funcionarios {funcionarios = f}) = getFuncionariosFromTuple f


getFuncionariosFromTuple :: [(String, Funcionario)] -> [Funcionario]
getFuncionariosFromTuple [] = []
getFuncionariosFromTuple ((_,f): cs) = f : getFuncionariosFromTuple cs

mapeiaCpf :: [Funcionario] -> [(String, Funcionario)]
mapeiaCpf [] = []
mapeiaCpf (f:cs)= (getCpf f, f) : mapeiaCpf cs


adicionaVendaFuncionario :: [Funcionario] -> String -> Venda -> Maybe [Funcionario]
adicionaVendaFuncionario [] cpfFuncionario novaVenda = Nothing
adicionaVendaFuncionario (Funcionario {nome = n, cpf= c, data_admissao =d, vendas =vend, salario=s}:cs) cpfFuncionario novaVenda
    | c == cpfFuncionario = Just ([Funcionario n c d (comp++[novaVenda]) s] ++ cs)
    | otherwise = adicionaVendaFuncionario cs cpfFuncionario novaVenda

excluiFuncionario :: Funcionarios -> Integer -> Integer -> Funcionarios 
excluiFuncionario [] _ _ = []
excluiFuncionario (o:os) cursor contador
   | cursor == contador = excluiFuncionario os cursor (contador+1)
   | otherwise = o:excluiFuncionario os cursor (contador+1)

getNomeFuncionario :: Funcionario -> String
getNomeFuncionario Funcionario {nomeFuncionario = n} = n

getDataDeAdmissao :: Funcionario -> String
getDataDeAdmissao Funcionario {data_admissao = n} = n

getVendas :: Funcionario -> [Venda]
getVendas Funcionario {vendas = n} = n


setSalario :: [Funcionario] -> String -> Double -> Maybe [Funcionario]
setSalario [] x novoSalario = Nothing
setSalario (s:cs) x novoSalario
    | cpf == x = Just ([Funcionario x nomeFuncionario vendas dataDeAdmissao salarioNovo] ++ cs)
    | otherwise = setSalario cs x novoSalario
    where
        cpfAtual = getCpf f
        nomeFuncionarioAtual = getNomeFuncionario f
        dataDeAdmissaoAtual = getDataDeAdmissao f
        validadeAtual = getValidade f

-----------------------------VISUALIZACAO-------------------------------

getFuncionariosEmLista :: IO [Produto]
getFuncionariosEmLista = do
    funcionarios <- openFile "../arquivos/Funcionarios.csv" ReadMode
    listaFuncionarios <- lines <$> hGetContents funcionarios
    return $ (converteEmLista listaFuncionarios)

converteEmLista :: [String] -> [Funcionario]
converteEmLista [] = []
converteEmLista (funcionario:lista) =
    converteEmFuncionario (split funcionario ',') : converteEmLista lista

converteEmFuncionario :: [String] -> Funcionario
converteEmFuncionario funcionario = Funcionario nome cpf data_admissao vendas salario
    where 
        cpf = (read (funcionario !! 0) :: String)
        nome = produto !! 1
        data_admissao = (read (funcionario !! 2) :: String)
        vendas = fromIO(getVendasEmLista id)
        salario = produto !! 3

getVendasEmLista :: Int -> IO [String]
getVendasEmLista id = do
    vendas <- openFile "../arquivos/Vendas.csv" ReadMode
    listaVendas <- lines <$> hGetContents vendas
    let vendasFinal = converteVendasEmLista listaVendas

-- Converte IO em puro
fromIO :: IO[String] -> [String]
fromIO x = (unsafePerformIO x :: [String])

-------------------------------UTIL----------------------------------------

formataParaEscrita :: [Funcionario] -> String
formataParaEscrita [] = []
formataParaEscrita (f:cs) = getAtributosFuncionario f ++ "\n" ++ formataParaEscrita cs

---------------------------IO FUNCIONARIO-----------------------------------

escreverArquivo :: Funcionarios -> IO ()
escreverArquivo funcionarioss = do
    arq <- openFile "../arquivos/Funcionarios.csv" WriteMode
    let dataFuncionarios = getFuncionarios funcionarios
    print "data"
    print dataFuncionarios
    hPutStrLn arq (formataParaEscrita dataFuncionarios)
    hClose arq