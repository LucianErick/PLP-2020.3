import System.IO
import Util
import Venda


data Funcionario = Funcionario {
    nomeFuncionario :: String,
    cpf :: String,
    data_admissao :: String,
    vendas :: [Venda],
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



---------------------------------------------------------------------------

excluiFuncionario :: Funcionarios -> Integer -> Integer -> Funcionarios 
excluiFuncionario [] _ _ = []
excluiFuncionario (o:os) cursor contador
   | cursor == contador = excluiFuncionario os cursor (contador+1)
   | otherwise = o:excluiFuncionario os cursor (contador+1)


---------------------------IO FUNCIONARIO-----------------------------------


EscreverArquivo :: funcionarios -> IO ()
EscreverArquivo funcionarios = do
               arq <- openFile "./arquivos/Funcionarios.txt" WriteMode
               hputStrLn arq (show (funcionarios))
               hClose arq
