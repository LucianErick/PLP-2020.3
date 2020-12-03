:- initialization(main).
:- include('Cliente.pl').
:- include('Funcionario.pl').
:- include('Produto.pl').
:- include('Arquivos.pl').


/* -> FUNCOES UTEIS -- */

up(119).
down(115).
left(97).
right(100).
select(113).
remotion(101).

/* -------------------- */
upAction(0, Limit, Limit).
upAction(Cursor, _, NewCursor) :-
    NewCursor is Cursor - 1.

downAction(Cursor, Limit, NewCursor) :-
    Max is Limit + 1,
    PC is Cursor + 1,
    NewCursor is PC mod Max.

remotionExitAction(Mensagem, Resultado) :-
    shell(clear),
    writeln(Mensagem),
    get_single_char(Input),
    (remotion(Input) -> Resultado is 1;
        Resultado is 0).

showOptions([], _, _).
showOptions([A|As], Cursor, Cursor) :- 
    write('-> '),
    writeln(A),
    N is Cursor + 1,
    showOptions(As, Cursor, N).
    
showOptions([A|As], Cursor, N) :- 
    write('   '),
    writeln(A),
    NewN is N + 1,
    showOptions(As, Cursor, NewN).

/* FUNCOES UTEIS <- */

/* -> FUNCOES PARA IO */

getString(FinalInput, Mensagem) :-
    write('\n'),
    writeln(Mensagem),
    read_line_to_codes(user_input, Entrada), atom_string(Entrada, Return),
    FinalInput = Return.

getDouble(FinalInput, Mensagem) :-
    write('\n'),
    writeln(Mensagem),
    read_line_to_codes(user_input, Entrada), atom_string(Entrada, Return),
    (number_string(Number, Return), Number >= 0 -> FinalInput = Number;
        getDouble(NewF, 'Entrada invalida! Tente digitar um número.\n Digite novamente!'), FinalInput is NewF).

getInt(FinalInput, Mensagem) :-
    write('\n'),
    writeln(Mensagem),
    read_line_to_codes(user_input, Entrada), atom_string(Entrada, Return),
    (number_string(Number, Return), Number >= 0 -> FinalInput = Number;
        getDouble(NewF, 'Entrada invalida! Tente digitar um número.\n digite novamente!'), FinalInput is NewF).

/* FUNCOES PARA IO <- */

/* -> TELA PRINCIPAL */

optionsMainScreen(['Entrar como gestor', 'Entrar como funcionario', 'Entrar como cliente', 'Sair']).
limitMain(3).

/* ------------------------------------------------------------------------------ */

showExitMessage() :-
    shell(clear),
    writeln('----------------------------------------------'),
    writeln('* MUITO OBRIGADO POR UTILIZAR O CORONA PHARM *'),
    writeln('*                VOLTE SEMPRE!               *'),
    writeln('----------------------------------------------\n').

doMainScreen(Cursor, Action) :-
    limitMain(Limit),
    (up(Action) -> upAction(Cursor, Limit, NewCursor), mainScreen(NewCursor);
     down(Action) -> downAction(Cursor, Limit, NewCursor), mainScreen(NewCursor);
     left(Action) -> showExitMessage();
     right(Action) -> (Cursor =:= 0 -> masterScreen(0);
                       Cursor =:= 1 -> employeeScreen(0);
                       Cursor =:= 2 -> clientScreen(0);
                       Cursor =:= 3 -> showExitMessage());
     mainScreen(Cursor)).

mainScreen(Cursor) :-
    shell(clear),
    writeln('\nBEM VINDO À CORONA PHARM!
             \n|| (w,s) Para mover o cursor  ||'),
    writeln('|| (a) Para modificar a tela  ||'),
    writeln('|| (d) Para entrar nas opcoes ||\n'),
    optionsMainScreen(ListaOpcoes),
    showOptions(ListaOpcoes, Cursor, 0),
    get_single_char(Action),
    doMainScreen(Cursor, Action).

% ---------------------------------------------TELA OPCOES GESTOR-----------------------------------------------------------

optionsMasterScreen(['Cadastrar produto', 'Cadastrar funcionario', 'Visualizar funcionários', 'Visualizar produtos', 'Visualizar clientes', 'Visualizar vendas gerais']).
limitMaster(5).

doMasterScreen(Cursor, Action) :-
    limitMaster(Limit),
    (up(Action) -> upAction(Cursor, Limit, NewCursor), masterScreen(NewCursor);
     down(Action) -> downAction(Cursor, Limit, NewCursor), masterScreen(NewCursor);
     left(Action) -> mainScreen(Cursor);
     right(Action) -> (Cursor =:= 0 -> registerProductScreen();
                       Cursor =:= 1 -> registerEmployeeScreen();
                       Cursor =:= 2 -> employeeViewScreen();
                       Cursor =:= 3 -> productViewScreen(0);
                       Cursor =:= 4 -> clientViewScreen();
                       Cursor =:= 5 -> sellViewScreen());
     masterScreen(Cursor)).

masterScreen(Cursor) :-
    shell(clear),
    writeln('\nBEM VINDO À CORONA PHARM!
                \n|| (w,s) Para mover o cursor  ||'),
    writeln('|| (a) Para modificar a tela  ||'),
    writeln('|| (d) Para entrar nas opcoes ||\n'),
    optionsMasterScreen(ListaOpcoes),
    showOptions(ListaOpcoes, Cursor, 0),
    get_single_char(Action),
    doMasterScreen(Cursor, Action).
% ---------------------------------------- TELA CADASTRAR PRODUTO ----------------------------------------
registerProductScreen() :-
    shell(clear),
    getInt(Id, 'Digite o ID do produto'),
    getString(Nome, 'Digite o nome do produto'),
    getDouble(Preco, 'Digite o preço do produto'),
    getString(Validade, 'Digite a data de validade do produto'),
    getString(Sintomas, 'Digite o sintoma (Apenas um)'),
    
    cadastraProduto(Id, Nome, Preco, Validade),
    cadastraSintomaProduto(Id, Sintomas),
    
    write('\nProduto cadastrado com sucesso!'),
    get_single_char(Action),
    masterScreen(0).

% ---------------------------------------- TELA CADASTRAR FUNCIONARIO -------------------------------------
registerEmployeeScreen() :-
    shell(clear),
    getString(Cpf, 'Digite o cpf do funcionário'),
    getString(Nome, 'Digite o nome do funcionário'),
    getDouble(Salario, 'Digite o salário do funcionário'),
    getString(Admissao, 'Digite a data de admissão do funcionário'),
    cadastraFuncionario(Cpf, Nome, Admissao, Salario),
    write('\nFuncionário cadastrado com sucesso!'),
    get_single_char(Action),
    masterScreen(0).

% ---------------------------------------- TELA VISUALIZAR CLIENTES ---------------------------------------
clientViewScreen() :-
    shell(clear),
    lerCsvRowList('Clientes.csv', Clientes),
    mostraClientes(Clientes),
    get_single_char(Action),
    mainScreen(0).
% ---------------------------------------- TELA VISUALIZAR FUNCIONARIOS -----------------------------------
employeeViewScreen() :-
    shell(clear),
    lerCsvRowList('Funcionarios.csv', Funcionarios),
    mostraFuncionarios(Funcionarios),
    get_single_char(Action),
    masterScreen(0).
% ---------------------------------------- TELA VISUALIZAR VENDAS -----------------------------------------
sellViewScreen() :-
    shell(clear),
    lerCsvRowList('ProdutosVenda.csv', Vendas),
    mostraProdutos(Vendas),
    get_single_char(Action),
    productViewScreen(0).

% --------------------------------------------------------------------------------------------------------
% --------------------------------------------TELA OPCOES FUNCIONARIO--------------------------------------------
optionsEmployeeScreen(['Cadastrar cliente', 'Cadastrar venda', 'Visualizar clientes', 'Visualizar produtos', 'Visualizar suas vendas']).
limitEmployee(4).

doEmployeeScreen(Cursor, Action) :-
    limitEmployee(Limit),
    (up(Action) -> upAction(Cursor, Limit, NewCursor), employeeScreen(NewCursor);
     down(Action) -> downAction(Cursor, Limit, NewCursor), employeeScreen(NewCursor);
     left(Action) -> mainScreen(Cursor);
     right(Action) -> (Cursor =:= 0 -> registerClientScreen();
                       Cursor =:= 1 -> registerSellScreen();
                       Cursor =:= 2 -> clientViewScreen();
                       Cursor =:= 3 -> productViewScreen(0);
                       Cursor =:= 4 -> employeeOwnSellsScreen());
     employeeScreen(Cursor)).

employeeScreen(Cursor) :-
    shell(clear),
    writeln('\nBEM VINDO À CORONA PHARM!
                \n|| (w,s) Para mover o cursor  ||'),
    writeln('|| (a) Para modificar a tela  ||'),
    writeln('|| (d) Para entrar nas opcoes ||\n'),
    optionsEmployeeScreen(ListaOpcoes),
    showOptions(ListaOpcoes, Cursor, 0),
    get_single_char(Action),
    doEmployeeScreen(Cursor, Action).
% ---------------------------------------- TELA CADASTRAR VENDAS -----------------------------------
registerSellScreen() :-
    shell(clear),
    getString(CpfFuncionario, 'Digite o cpf do funcionario'),
    getString(Produto, 'Digite o id do produto'),
    getString(CpfCliente, 'Digite o cpf do cliente'),
    getString(DataVenda, 'Digite a data da venda'),

    adicionaVenda(CpfFuncionario, IdProduto, CpfCliente, DataVenda),

    write('\nVenda cadastrada com sucesso!'),
    get_single_char(Action),
    employeeScreen(0).
% ---------------------------------------- TELA VISUALIZAR SUAS PROPRIAS VENDAS --------------------------
employeeOwnSellsScreen() :-
    shell(clear),
    getString(CpfFuncionario, 'Digite o CPF do funcionário'),
    mostraQTDVendasFuncionario(CpfFuncionario),
    get_single_char(Action),
    employeeScreen(0).

% ---------------------------------------- TELA CADASTRAR CLIENTE -----------------------------------------
registerClientScreen() :-
    shell(clear),
    getString(Cpf, 'Digite o cpf do cliente'),
    getString(Nome, 'Digite o nome do cliente'),
    getString(DataCadastro, 'Digite a data de cadastro do cliente'),
    cadastraCliente(Cpf, Nome, DataCadastro),
    write('\nCliente cadastrado com sucesso!'),
    get_single_char(Action),
    employeeScreen(0).
% -----------------------------------------TELA OPCOES CLIENTE--------------------------------------

optionsClientScreen(['Visualizar produtos', 'Comprar produto']).
limitClient(1).

doClientScreen(Cursor, Action) :-
    limitClient(Limit),
    (up(Action) -> upAction(Cursor, Limit, NewCursor), clientScreen(NewCursor);
     down(Action) -> downAction(Cursor, Limit, NewCursor), clientScreen(NewCursor);
     left(Action) -> mainScreen(Cursor);
     right(Action) -> (Cursor =:= 0 -> productViewScreen(0);
                       Cursor =:= 1 -> registerBuyScreen());
     clientScreen(Cursor)).

clientScreen(Cursor) :-
    shell(clear),
    writeln('\nBEM VINDO À CORONA PHARM!
                \n|| (w,s) Para mover o cursor  ||'),
    writeln('|| (a) Para modificar a tela  ||'),
    writeln('|| (d) Para entrar nas opcoes ||\n'),
    optionsClientScreen(ListaOpcoes),
    showOptions(ListaOpcoes, Cursor, 0),
    get_single_char(Action),
    doClientScreen(Cursor, Action).

registerBuyScreen() :-
    shell(clear),
    getString(CpfCliente, 'Digite o cpf do cliente'),
    getInt(IdProduto, 'Digite o id do produto'),
    adicionaCompra(CpfCliente, IdProduto),
    write('\nCompra cadastrada com sucesso!'),
    get_single_char(Action),
    clientScreen(0).

% ---------------------------------- TELA OPCOES VISUALIZAR PRODUTOS --------------------------------

optionsProductViewScreen(['Visualizar por sintoma', 'Visualizar por existente no estoque']).
limitProductView(1).

doProductViewScreen(Cursor, Action) :-
    limitProductView(Limit),
    (up(Action) -> upAction(Cursor, Limit, NewCursor), productViewScreen(NewCursor);
     down(Action) -> downAction(Cursor, Limit, NewCursor), productViewScreen(NewCursor);
     left(Action) -> mainScreen(Cursor);
     right(Action) -> (Cursor =:= 0 -> productSymptomViewScreen();
                       Cursor =:= 1 -> productStockViewScreen());
     productViewScreen(Cursor)).

productViewScreen(Cursor) :-
    shell(clear),
    writeln('\nBEM VINDO À CORONA PHARM!
                \n|| (w,s) Para mover o cursor  ||'),
    writeln('|| (a) Para modificar a tela  ||'),
    writeln('|| (d) Para entrar nas opcoes ||\n'),
    optionsProductViewScreen(ListaOpcoes),
    showOptions(ListaOpcoes, Cursor, 0),
    get_single_char(Action),
    doProductViewScreen(Cursor, Action).
%------------------------------------------ Por sintoma
productSymptomViewScreen() :-
    shell(clear),
    lerCsvRowList('SintomasProdutos.csv', SintomasProdutos),
    mostraSintomaProduto(SintomasProdutos),
    get_single_char(Action),
    productViewScreen(0).

%------------------------------------------ Por estoque
productStockViewScreen() :-
    shell(clear),
    lerCsvRowList('Produtos.csv', Produtos),
    mostraProdutos(Produtos),
    get_single_char(Action),
    productViewScreen(0).

% -----------------------------------------TELA SAIR------------------------------------------------------
   
main :-
    mainScreen(0),
    halt(0).