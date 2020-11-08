import System.IO 
import System.IO.Unsafe
import Control.Exception
import System.IO.Error 
import System.Process
import Control.Monad (when)
import Text.Printf
import System.IO.Unsafe

-----------------------------------------------------------------------------------------------------
import Produto
import Funcionario
import Cliente
import Venda
import Util
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

lerEntradaString :: IO String
lerEntradaString = do
         hSetBuffering stdin LineBuffering
         hSetEcho stdin True
         x <- getLine
         return x

lerEntradaInt :: IO Int
lerEntradaInt = do
         hSetBuffering stdin LineBuffering
         hSetEcho stdin True
         x <- readLn
         return x

lerEntradaDouble :: IO Double
lerEntradaDouble = do
         hSetBuffering stdin LineBuffering
         hSetEcho stdin True
         x <- readLn
         return x
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
mudarTelaInicial cursor                          | cursor == 0 = do telaOpcoesGestor 0
                                                 | cursor == 1 = do telaOpcoesFuncionario 0            
                                                 | cursor == 2 = do telaOpcoesCliente 0
                                                 | cursor == 3 = do telaSair


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

mudarTelaOpcoesGestor :: Integer -> IO ()
mudarTelaOpcoesGestor cursor
   | cursor == 0 = cadastroProdutosTela
   | cursor == 1 = cadastroFuncionarioTela
   | cursor == 2 = mudarPrecoProdutoTela
   | cursor == 3 = telaOpcoesVisualizarProdutos 0
   | cursor == 4 = visualizarClientesTela
   | cursor == 5 = visualizarVendasTela

doOpcoesGestor :: Integer -> [Char] -> IO ()
doOpcoesGestor cursor action | action == "\ESC[B" = telaOpcoesGestor ((cursor+1) `mod` 6)
                                                    | action == "\ESC[A" && cursor /= 0 = telaOpcoesGestor (cursor-1)
                                                    | action == "\ESC[A" && cursor == 0 = telaOpcoesGestor 5
                                                    | action == "\ESC[C" = mudarTelaOpcoesGestor cursor
                                                    | action == "\ESC[D" = telaInicial 0
                                                    | otherwise = telaOpcoesGestor cursor


telaOpcoesGestor :: Integer -> IO ()
telaOpcoesGestor cursor = do
   
   system "clear"
   putStrLn ("Bem vindo, GESTOR! \n\n|| Aperte (Seta Direita) para escolher qual opcão acessar e (Seta Esquerda) para voltar à tela anterior. ||\n")
   showSimpleScreen opcoesTelaGestor cursor 0
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   doOpcoesGestor cursor action

-- Cadastrar produto
cadastroProdutosTela :: IO ()
cadastroProdutosTela = do
   system "clear"

   putStrLn ("Digite o id do produto:")
   id <- lerEntradaInt

   putStrLn ("\nDigite o nome do produto:")
   nome <- lerEntradaString

   putStrLn ("\nDigite o preço do produto:")
   preco <- lerEntradaDouble

   putStrLn ("\nDigite os sintomas do produto (separados por ','):")
   sintomas <- lerEntradaString
   let listaSintomas = split sintomas ','

   putStrLn ("\nDigite a validade do produto (com '/'):")
   validade <- lerEntradaString

   let produto = Produto id nome preco listaSintomas validade 

   let listaProdutos = [produto]
   let adicionarProduto = listaProdutos
   escreverArquivo adicionarProduto
   
   putStrLn("\nO produto foi cadastrado com sucesso!\n(Mas não se preocupe, ele pode ser modificado futuramente!)")
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   putStrLn (" ")

   telaInicial 0

-- Cadastrar funcionario
listaVendasInicialFuncionario:: [String]
listaVendasInicialFuncionario = []

cadastroFuncionarioTela :: IO ()
cadastroFuncionarioTela = do
   system "clear"

   putStrLn ("Digite o nome do funcionário:")
   nomeFuncionario <- lerEntradaString

   putStrLn ("\nDigite o cpf do funcionário:")
   cpfFuncionario <- lerEntradaString

   putStrLn ("\nDigite a data de admissão do funcionário (usando '/'):")
   dataAdmissao <- lerEntradaString

   putStrLn ("\nDigite o salário do funcionário:")
   salario <- lerEntradaDouble

   putStrLn("\nO funcionário foi cadastrado com sucesso!\n(Mas não se preocupe, ele pode ser modificado futuramente!)\n")
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False

   let funcionario = Funcionario nomeFuncionario cpfFuncionario dataAdmissao listaVendasInicialFuncionario salario 

   let listaFuncionarios = [funcionario]
   let adicionarFuncionario = listaFuncionarios
   escreverArquivoFuncionario adicionarFuncionario

   action <- getKey
   putStrLn (" ")
   telaInicial 0

-- Atualizar Preço
mudarPrecoProdutoTela :: IO () -- Falta colocar todos os parâmetros p realmente ser funcional
mudarPrecoProdutoTela = do
   system "clear"

   putStrLn ("\nDigite o id do produto que você deseja alterar:")
   idAtual <- lerEntradaInt

   putStrLn ("\nDigite o novo preço do produto:")
   novoPreco <- lerEntradaDouble

   -- let listaProdutos = fromIOProduto getProdutosEmLista
   --return setPreco listaProdutos idAtual novoPreco
   
   let produtosNovo = setPreco produtos idAtual novoPreco

   putStrLn("\nO preço do produto foi atualizado com sucesso!\n")
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False

-- Visualizar Clientes
visualizarClientesTela :: IO ()
visualizarClientesTela = do
      system "clear"
      clientes <- openFile "../arquivos/Clientes.csv" ReadMode
      listaClientes <- lines <$> hGetContents clientes
      print listaClientes
      
      action <- getKey
      putStrLn (" ")
      telaInicial 0


-- Visualizar vendas
visualizarVendasTela :: IO ()
visualizarVendasTela = do
      system "clear"
      vendas <- openFile "../arquivos/Vendas.csv" ReadMode
      listaVendas <- lines <$> hGetContents vendas
      print listaVendas
      
      action <- getKey
      putStrLn (" ")
      telaInicial 0

--------------------------------------------------------------------------------------
--Tela funcionario

opcoesTelaFuncionario :: [String]
opcoesTelaFuncionario = ["Cadastrar cliente", "Cadastrar venda", "Visualizar clientes", "Visualizar produtos"]

mudarTelaOpcoesFuncionario :: Integer -> IO ()
mudarTelaOpcoesFuncionario cursor
   | cursor == 0 = cadastroClienteTela
   | cursor == 1 = cadastroVendaTela
   | cursor == 2 = visualizarClientesTela
   | cursor == 3 = telaOpcoesVisualizarProdutos 0

doOpcoesFuncionario :: Integer -> [Char] -> IO ()
doOpcoesFuncionario cursor action | action == "\ESC[B" = telaOpcoesFuncionario ((cursor+1) `mod` 4)
                                                    | action == "\ESC[A" && cursor /= 0 = telaOpcoesFuncionario (cursor-1)
                                                    | action == "\ESC[A" && cursor == 0 = telaOpcoesFuncionario 3
                                                    | action == "\ESC[C" = mudarTelaOpcoesFuncionario cursor
                                                    | action == "\ESC[D" = telaInicial 0
                                                    | otherwise = telaOpcoesFuncionario cursor

telaOpcoesFuncionario :: Integer -> IO ()
telaOpcoesFuncionario cursor = do
   
   system "clear"
   putStrLn ("Bem vindo, FUNCIONÁRIO! \n\n|| Aperte (Seta Direita) para escolher qual opcão acessar e (Seta Esquerda) para voltar à tela anterior. ||\n")
   showSimpleScreen opcoesTelaFuncionario cursor 0
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   doOpcoesFuncionario cursor action

-- Cadastrar Cliente
listaComprasInicialCliente:: [Produto]
listaComprasInicialCliente = []

cadastroClienteTela :: IO ()
cadastroClienteTela = do
   system "clear"

   putStrLn ("Digite o nome do cliente:")
   nomeCliente <- lerEntradaString

   putStrLn ("\nDigite o cpf do cliente:")
   cpfCliente <- lerEntradaString -- tem que ajeitar isso

   putStrLn ("\nDigite a data de cadastro do cliente (usando '/'):")
   dataCadastro <- lerEntradaString

   putStrLn ("\nDigite os sintomas do cliente:")
   sintomasCliente <- lerEntradaString
   let listaSintomasCliente = split sintomasCliente ','

   putStrLn("\nO cliente foi cadastrado com sucesso!\n(Mas não se preocupe, ele pode ser modificado futuramente!)")
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False

   let cliente = Cliente nomeCliente cpfCliente dataCadastro listaComprasInicialCliente listaSintomasCliente

   let listaCliente = [cliente]
   let adicionarCliente = listaCliente
   escreverArquivoCliente adicionarCliente
   
   action <- getKey
   telaInicial 0

-- Cadastrar Venda

cadastroVendaTela :: IO () -- Falta colocar todos os parâmetros p realmente ser funcional
cadastroVendaTela = do
   system "clear"

   putStrLn ("Digite o id da venda:")
   idVenda <- lerEntradaString

   putStrLn ("\nDigite o cpf do cliente:")
   cpfCliente <- lerEntradaString -- tem que ajeitar isso
   
   putStrLn ("\nDigite o cpf do funcionário:")
   cpfFuncionario <- lerEntradaString -- tem que ajeitar isso

   putStrLn ("\nDigite a data de cadastro da venda (usando '/'):")
   dataVenda <- lerEntradaString

   putStrLn ("\nDigite os ids dos produtos (separados por ','):")
   produtosVendidos <- lerEntradaString
   let listaProdutos = split produtosVendidos ','

   putStrLn("\nA venda foi cadastrada com sucesso!\n")

   let venda = Venda idVenda cpfCliente cpfFuncionario dataVenda listaProdutos
   let listaVenda = [venda]
   let adicionarVenda = listaVenda
   escreverArquivoVendas adicionarVenda
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   
   action <- getKey
   putStrLn(" ")
   telaInicial 0

-------------------------------------------------------------------------------------------------------
-- Tela cliente

opcoesTelaCliente :: [String]
opcoesTelaCliente = ["Visualizar produtos", "Comprar produto"]

mudarTelaOpcoesCliente :: Integer -> IO ()
mudarTelaOpcoesCliente cursor
   | cursor == 0 = telaOpcoesVisualizarProdutos 0--
   | cursor == 1 = comprarProdutoTela--

-- Ajeitar isso, nao sei se funciona normalmente se colocar uma opcao a mais
doOpcoesCliente :: Integer -> [Char] -> IO ()
doOpcoesCliente cursor action | action == "\ESC[B" = telaOpcoesCliente ((cursor+1) `mod` 2)
                                                    | action == "\ESC[A" && cursor /= 0 = telaOpcoesCliente (cursor-1)
                                                    | action == "\ESC[A" && cursor == 0 = telaOpcoesCliente 1
                                                    | action == "\ESC[C" = mudarTelaOpcoesCliente cursor
                                                    | action == "\ESC[D" = telaInicial 0
                                                    | otherwise = telaOpcoesCliente cursor

telaOpcoesCliente :: Integer -> IO ()
telaOpcoesCliente cursor = do
   
   system "clear"
   putStrLn ("Bem vindo, CLIENTE \n\n| Aperte (Seta Direita) para escolher qual opcão acessar e (Seta Esquerda) para voltar à tela anterior. ||\n")
   showSimpleScreen opcoesTelaCliente cursor 0
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   doOpcoesCliente cursor action

-- Comprar produto

comprarProdutoTela :: IO ()
comprarProdutoTela = do
   system "clear"

   putStrLn ("Digite os ids dos produtos (separados por vírgula):")
   produtos <- lerEntradaString
   
   putStrLn ("\nDigite os sintomas (separados por vírgula):")
   sintomas <- lerEntradaString -- tem que ajeitar isso

   putStrLn("\nA compra foi cadastrada com sucesso! Volte sempre!\n")

   escreverComprasCliente produtos sintomas
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   
   action <- getKey
   putStrLn(" ")
   telaInicial 0




-----------------------------------------------------------------------------------------------------------
-- Tela visualizar produtos

opcoesTelaVisualizarProdutos :: [String]
opcoesTelaVisualizarProdutos = ["Visualizar por sintoma", "Visualizar por existentes no estoque"]

mudarTelaOpcoesVisualizarProdutos :: Integer -> IO ()
mudarTelaOpcoesVisualizarProdutos cursor
   | cursor == 0 = visualizarProdutosEstoqueTela--
   | cursor == 1 = visualizarProdutosSintomasTela--

doOpcoesVisualizarProdutos :: Integer -> [Char] -> IO ()
doOpcoesVisualizarProdutos cursor action | action == "\ESC[B" = telaOpcoesVisualizarProdutos ((cursor+1) `mod` 2)
                                                    | action == "\ESC[A" && cursor /= 0 = telaOpcoesVisualizarProdutos (cursor-1)
                                                    | action == "\ESC[A" && cursor == 0 = telaOpcoesVisualizarProdutos 1
                                                    | action == "\ESC[C" = mudarTelaOpcoesVisualizarProdutos cursor
                                                    | action == "\ESC[D" = telaInicial 0 -- n sei se eh tela Inicial mesmo
                                                    | otherwise = telaOpcoesVisualizarProdutos cursor

telaOpcoesVisualizarProdutos :: Integer -> IO ()
telaOpcoesVisualizarProdutos cursor = do
   
   system "clear"
   putStrLn ("Bem vindo à seção de visualzação de produtos!\n\n|| Aperte (Seta Direita) para escolher qual opcão acessar e (Seta Esquerda) para voltar à tela inicial. ||\n")

   showSimpleScreen opcoesTelaVisualizarProdutos cursor 0
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   doOpcoesVisualizarProdutos cursor action

-- Visualizar produtos pelo estoque
visualizarProdutosEstoqueTela :: IO ()
visualizarProdutosEstoqueTela = do
      system "clear"
      
      produtos <- openFile "../arquivos/Produtos.csv" ReadMode
      
      listaProdutos <- lines <$> hGetContents produtos
      print (produtos)

      action <- getKey
      putStrLn (" ")
      telaInicial 0


-- Visualizar produtos pelos sintomas
visualizarProdutosSintomasTela :: IO ()
visualizarProdutosSintomasTela = do
      system "clear"
      produtos <- openFile "../arquivos/SintomasProduto.csv" ReadMode
      listaProdutos <- lines <$> hGetContents produtos
      print (produtos)
      
      action <- getKey
      putStrLn (" ")
      telaInicial 0

-----------------------------------------------------------------------------------------------------------

doTelaSair :: String -> IO ()
doTelaSair action    | action == "s" = return() 
                     | otherwise = telaInicial 0
    
telaSair :: IO ()
telaSair = do
   system "clear"
   putStrLn("Digite (s) para encerrar a execucao ou (Outra tecla) para voltar para o menu");
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   doTelaSair action

-----------------------------------------------------------------------------------------------------------
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