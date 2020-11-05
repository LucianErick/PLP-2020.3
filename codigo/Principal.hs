import System.IO 
import Control.Exception
import System.IO.Error 
import System.Process
import Control.Monad (when)
import Text.Printf

-----------------------------------------------------------------------------------------------------

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

showSimpleScreen :: [String] -> Integer -> Integer -> IO()
showSimpleScreen [] cursor contador = return ()
showSimpleScreen (o:os) cursor contador = do
   if contador == cursor
      then 
      putStrLn("->" ++ o)
   else
      putStrLn("  " ++ o)
   showSimpleScreen os cursor (contador+1)

-----------------------------------------------------------------------------------------------------

opcoesTelaInicial :: [String]
opcoesTelaInicial = ["Entrar como gestor", "Entrar como funcionário", "Entrar como cliente", "Sair"]

doTelaInicial :: Integer -> [Char] -> IO ()
doTelaInicial cursor action | action == "\ESC[B" = telaInicial ((cursor+1) `mod` 4)
                                                    | action == "\ESC[A" && cursor /= 0 = telaInicial (cursor-1)
                                                    | action == "\ESC[A" && cursor == 0 = telaInicial 3
                                                    | action == "\ESC[C" = mudarTelaInicial cursor
                                                    | action == "\ESC[D" = putStrLn("Sair")
                                                    | otherwise = telaInicial cursor

mudarTelaInicial :: Integer -> IO()
mudarTelaInicial cursor                          | cursor == 0 = do return() 
                                                 | cursor == 1 = do return()            
                                                 | cursor == 2 = do return() 
                                                 | cursor == 3 = do return()


telaInicial :: Integer -> IO ()
telaInicial cursor = do
   
   system "clear"
   putStrLn("Bem-vindo à farmácia Corona Pharm!")
   putStrLn("Como desejas acessar?\n")
   putStrLn("\n|| Utilize os direcionais do teclado para mover o cursor ||\n")
   showSimpleScreen opcoesTelaInicial cursor 0
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   doTelaInicial cursor action

-------------------------------------------------------------------------------------
-- Tela do gestor

opcoesTelaGestor :: [String]
opcoesTelaGestor = ["Cadastrar produto", "Cadastrar funcionário", "Atualizar preço", "Visualizar produtos", "Visualizar clientes", "Visualizar vendas"]

mudarTelaOpcoesGestor :: Produto -> Funcionario -> Cliente -> Integer -> IO ()
mudarTelaOpcoesGestor produtos funcionarios clientes cursor
   | cursor == 0 = telaCadastroProduto produtos
   | cursor == 1 = telaCadastroFuncionario funcionarios
   | cursor == 2 = telaAtualizarPreco produtos
   | cursor == 3 = telaVisualizarProdutos produtos
   | cursor == 4 = telaVisualizarClientes clientes
   | cursor == 5 = telaVisualizarVendas vendas

-- Ajeitar isso, nao sei se funciona normalmente se colocar uma opcao a mais
doOpcoesGestor :: Produto -> Funcionario -> Cliente -> Integer -> [Char] -> IO ()
doOpcoesGestor produtos funcionarios clientes cursor action | action == "\ESC[B" = configuracoesScreen disciplinas compromissos ((cursor+1) `mod` 4)
                                                    | action == "\ESC[A" && cursor /= 0 = configuracoesScreen disciplinas compromissos (cursor-1)
                                                    | action == "\ESC[A" && cursor == 0 = configuracoesScreen disciplinas compromissos 3
                                                    | action == "\ESC[C" = mudarTelaOpcoesGestor disciplinas compromissos cursor
                                                    | action == "\ESC[D" = mainScreen disciplinas compromissos 0
                                                    | otherwise = configuracoesScreen disciplinas compromissos cursor


telaOpcoesGestor :: Produto -> Funcionario -> Cliente -> Integer -> IO ()
telaOpcoesGestor produtos funcionarios clientes cursor = do
   
   system "clear"
   putStrLn ("Bem vindo, GESTOR \n|| Aperte (Seta Direita) para escolher qual opcão acessar ||\n")
   showSimpleScreen opcoesTelaGestor cursor 0
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   doOpcoesGestor produtos funcionarios clientes cursor action


-- Cadastrar produto
-- Ver quais são as funções de get para ler em cada uma das opcoes.
telaCadastroProduto :: Produto -> IO ()
telaCadastroProduto produtos = do


   system "clear"

   nome <- getNovoNomeDisciplina
   professor <- getNovoProfessorDisciplina
   sala <- getNovaSalaDisciplina

   putStrLn("\n Produto Cadastrado com Sucesso.\nobs: Não se preocupe, você pode modificar o preço futuramente.")
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey

   

   configuracoesScreen (disciplinas++[(Disciplina nome professor sala nota)]) compromissos 0
   
   putStrLn("")
-- Cadastrar funcionário

-- Atualizar preço produto

-- Visualizar Produtos

-- Visualizar clientes

-- Visualizar vendas


doTelaInicial :: Integer -> [Char] -> IO ()
doTelaInicial cursor action | action == "\ESC[B" = telaInicial ((cursor+1) `mod` 4)
                                                    | action == "\ESC[A" && cursor /= 0 = telaInicial (cursor-1)
                                                    | action == "\ESC[A" && cursor == 0 = telaInicial 3
                                                    | action == "\ESC[C" = mudarTelaInicial cursor
                                                    | action == "\ESC[D" = putStrLn("Sair")
                                                    | otherwise = telaInicial cursor

mudarTelaInicial :: Integer -> IO()
mudarTelaInicial cursor                          | cursor == 0 = do return() 
                                                 | cursor == 1 = do return()            
                                                 | cursor == 2 = do return() 
                                                 | cursor == 3 = do return()


telaInicial :: Integer -> IO ()
telaInicial cursor = do
   
   system "clear"
   putStrLn("Bem-vindo à farmácia Corona Pharm!")
   putStrLn("Como desejas acessar?\n")
   putStrLn("\n|| Utilize os direcionais do teclado para mover o cursor ||\n")
   showSimpleScreen opcoesTelaInicial cursor 0
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   doTelaInicial cursor action


--------------------------------------------------------------------------------------

opcoesTelaFuncionario :: [String]
opcoesTelaFuncionario = ["Cadastrar cliente", "Cadastrar venda", "Visualizar clientes", "Visualizar produtos", "Visualizar lista de suas vendas"]

opcoesTelaCliente :: [String]
opcoesTelaCliente = ["Visualizar produtos", "Comprar produto"]

opcoesTelaVisualizarProdutos :: [String]
opcoesTelaVisualizarProdutos = ["Visualizar por sintoma", "Visualizar por existentes no estoque"]

----------------------------------SISTEM RESET----------------------------------------

resetSystemScreen :: Clientes -> Funcionarios -> Produtos -> SintomasProduto -> IO ()
resetSystemScreen clientes funcionarios produtos sintomas = do
   system "clear"
   putStrLn ("CASO DESEJE RESETAR O SISTEMA APERTE (a)")
   
   hSetBuffering stdin LineBuffering
   hSetEcho stdin True
   x <- getLine

   if (x == "a") then do
      putStrLn ("SISTEMA RESETADO COM SUCESSO")
      x <- getLine
      configuracoesScreen [] [] 0
      
   else do
      putStrLn ("SISTEMA NAO RESETADO")
      x <- getLine
      configuracoesScreen clientes funcionarios produtos sintomas 0

------------------------------------SISTEM ON----------------------------------------

run :: IO ()
run = do
   {catch (iniciar) error;}
   where
      iniciar = do
      {
         telaInicial 0;
         return ()
      }
      error = ioError 

main :: IO ()
main = do
   run