import System.IO 
import Control.Exception
import System.IO.Error 
import System.Process
import Control.Monad (when)
import Text.Printf
 
type Funcionarios = [Funcionario]                                                   
type NomeFuncionario = String
type Data = String
type Salario = Double
data Funcionario = Funcionario nomeFuncionario data salario

------------------------------------------------------------------------

getNovoNomeDoFuncionario :: IO Double
getNovoNomeDeFuncionario = do 
   putStrLn("\nDigite um Nome do novo funcionário")
   hSetBuffering stdin LineBuffering
   hSetEcho stdin True
   x <- readLn
   return x

getNovoSalarioDoFuncionario :: IO Double
getNovoPesoNota = do 
   putStrLn("\nDigite um Salário para o novo funcionário")
   hSetBuffering stdin LineBuffering
   hSetEcho stdin True
   x <- readLn
   return x

getNovaDataDeAdmissaoDoFuncionario :: IO String
getNovoNomeDeFuncionario = do 
   putStrLn("\nDigite uma nova Data de admissão do novo funcionário")
   hSetBuffering stdin LineBuffering
   hSetEcho stdin True
   x <- readLn
   return x

--------------------------------------------------------------------------

changeNomeFuncionario :: Funcionarios -> Integer -> Integer -> String -> Funcionarios
changeNomeFuncionario [] nomeFuncionario contador novoNomeFuncionario = []
changeNomeFuncionario ((Funcionario nomeFuncionario Data Salario):os) nomeFuncionario contador novoNomeFuncionario
   | contador == nomeFuncionario = (Funcionario novoNomeFuncionario Data Salario) : changeNomeFuncionario os nomeFuncionario  (contador+1) novoNomeFuncionario
   | otherwise = (Funcionario NomeFuncionario Data Salario) : changeNomeFuncionario os nomeFuncionario (contador+1) novoNomeFuncionario

changeAdmissaoFuncionario :: Funcionarios -> Integer -> Integer -> String -> Funcionarios
changeAdmissaoFuncionario [] nomeFuncionario contador novaDataAdmissao = []
changeAdmissaoFuncionario ((Funcionario nomeFuncionario Data Salario):os) nomeFuncionario contador novaDataAdmissao
   | contador == nomeFuncionario = (Funcionario NomeFuncionario novaDataAdmissao Salario) : changeAdmissaoFuncionario os nomeFuncionario  (contador+1) novaDataAdmissao
   | otherwise = (Funcionario NomeFuncionario Data Salario) : changeAdmissaoFuncionario os nomeFuncionario (contador+1) novaDataAdmissao

changeSalarioDoFuncionario :: Funcionarios -> Integer -> Integer -> Double -> Funcionarios
changeSalarioDoFuncionario [] nomeFuncionario contador novoSalario = []
changeSalarioDoFuncionario ((Funcionario nomeFuncionario Data Salario):os) nomeFuncionario contador novoSalario
   | contador == nomeFuncionario = (Funcionario NomeFuncionario Data novoSalario) : changeSalarioDoFuncionario os nomeFuncionario  (contador+1) novoSalario
   | otherwise = (Funcionario NomeFuncionario Data Salario) : changeSalarioDoFuncionario os nomeFuncionario (contador+1) novoSalario

---------------------------------------------------------------------------

excluiFuncionario :: Funcionarios -> Integer -> Integer -> Funcionarios 
excluiFuncionario [] _ _ = []
excluiFuncionario (o:os) cursor contador
   | cursor == contador = excluiFuncionario os cursor (contador+1)
   | otherwise = o:excluiFuncionario os cursor (contador+1)


EscreverArquivo :: funcionarios -> IO ()
EscreverArquivo funcionarios = do
               arq <- openFile "./arquivos/Funcionarios.txt" WriteMode
               hputStrLn arq (show (funcionarios))
               hClose arq
