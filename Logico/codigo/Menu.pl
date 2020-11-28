:- initialization(main).
:- include('Cliente.pl').
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

switch([O|Os], Item, Pos, Cont, NewO) :-
    (Pos =:= Cont -> NewO = [Item|Os];
        Cont2 is Cont + 1, switch(Os, Item, Pos, Cont2, NewO2), NewO = [O|NewO2]).

remove([], _, _, []).
remove([O|Os], Pos, Cont, NewO) :-
    (Pos =:= Cont -> NewO = Os;
        Cont2 is Cont + 1, remove(Os, Pos, Cont2, NewO2), NewO = [O|NewO2]).

add([], Item, Item).
add([O|Os], Item, NewO) :-
    add(Os, Item, NewO2),
    NewO = [O|NewO2].

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
        getDouble(NewF, 'Entrada invalida digite novamente!'), FinalInput is NewF).

getInt(FinalInput, Mensagem) :-
    write('\n'),
    writeln(Mensagem),
    read_line_to_codes(user_input, Entrada), atom_string(Entrada, Return),
    (number_string(Number, Return), Number >= 0 -> FinalInput = Number;
        getDouble(NewF, 'Entrada invalida digite novamente!'), FinalInput is NewF).

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

% ---------------------------------------------TELA GESTOR-----------------------------------------------------------

optionsMasterScreen(['Cadastrar produto', 'Cadastrar funcionario', 'Atualizar preço', 'Visualizar funcionários', 'Visualizar produtos', 'Visualizar clientes', 'Visualizar vendas gerais']).
limitMaster(6).

doMasterScreen(Cursor, Action) :-
    limitMaster(Limit),
    (up(Action) -> upAction(Cursor, Limit, NewCursor), masterScreen(NewCursor);
     down(Action) -> downAction(Cursor, Limit, NewCursor), masterScreen(NewCursor);
     left(Action) -> mainScreen(Cursor);
     right(Action) -> (Cursor =:= 0 -> write('Voce cadastrou um produto');%acessoDisciplinasScreen(ListaCompromissos, ListaDisciplinas, 0);
                       Cursor =:= 1 -> write('Voce cadastrou um funcionario');%acessoCompromissosScreen(ListaCompromissos, ListaDisciplinas, 0);
                       Cursor =:= 2 -> write('Voce atualizou o preço');%configuracoesScreen(ListaCompromissos, ListaDisciplinas, 0);
                       Cursor =:= 3 -> write('Voce visualizou os funcionarios');%tutorialScreen(ListaCompromissos, ListaDisciplinas));
                       Cursor =:= 4 -> productViewScreen(0);
                       Cursor =:= 5 -> clientViewScreen();
                       Cursor =:= 6 -> write('Voce visualizou as vendas'));
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

% ---------------------------------------- TELA VISUALIZAR CLIENTES -----------------------------------
clientViewScreen() :-
    shell(clear),
    lerCsvRowList('Clientes.csv', Clientes),
    mostraClientes(Clientes),
    get_single_char(Action),
    masterScreen(0).


























% --------------------------------------------TELA FUNCIONARIO--------------------------------------------
optionsEmployeeScreen(['Cadastrar cliente', 'Cadastrar venda', 'Visualizar clientes', 'Visualizar produtos', 'Visualizar suas vendas']).
limitEmployee(4).

doEmployeeScreen(Cursor, Action) :-
    limitEmployee(Limit),
    (up(Action) -> upAction(Cursor, Limit, NewCursor), employeeScreen(NewCursor);
     down(Action) -> downAction(Cursor, Limit, NewCursor), employeeScreen(NewCursor);
     left(Action) -> mainScreen(Cursor);
     right(Action) -> (Cursor =:= 0 -> write('Voce cadastrou um cliente');%acessoDisciplinasScreen(ListaCompromissos, ListaDisciplinas, 0);
                       Cursor =:= 1 -> write('Voce cadastrou uma venda');%acessoCompromissosScreen(ListaCompromissos, ListaDisciplinas, 0);
                       Cursor =:= 2 -> clientViewScreen();%configuracoesScreen(ListaCompromissos, ListaDisciplinas, 0);
                       Cursor =:= 3 -> productViewScreen(0);%tutorialScreen(ListaCompromissos, ListaDisciplinas));
                       Cursor =:= 4 -> write('Voce visualizou sua lista de vendas'));
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


% -----------------------------------------TELA CLIENTE---------------------------------------------------

optionsClientScreen(['Visualizar produtos', 'Comprar produto']).
limitClient(1).

doClientScreen(Cursor, Action) :-
    limitClient(Limit),
    (up(Action) -> upAction(Cursor, Limit, NewCursor), clientScreen(NewCursor);
     down(Action) -> downAction(Cursor, Limit, NewCursor), clientScreen(NewCursor);
     left(Action) -> mainScreen(Cursor);
     right(Action) -> (Cursor =:= 0 -> productViewScreen(0);
                       Cursor =:= 1 -> write('Voce realizou uma compra'));
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
% --------------------------------------Tela Visualizar Produtos------------------------------------------

optionsProductViewScreen(['Visualizar por sintoma', 'Visualizar por existente no estoque']).
limitProductView(1).

doProductViewScreen(Cursor, Action) :-
    limitProductView(Limit),
    (up(Action) -> upAction(Cursor, Limit, NewCursor), productViewScreen(NewCursor);
     down(Action) -> downAction(Cursor, Limit, NewCursor), productViewScreen(NewCursor);
     left(Action) -> mainScreen(Cursor);
     right(Action) -> (Cursor =:= 0 -> write('Voce visualizou os produtos por sintoma');
                       Cursor =:= 1 -> write('Voce visualizou os produtos por estoque'));
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

% -----------------------------------------TELA SAIR------------------------------------------------------
   
main :-
    % readDisciplinas(ListaDisciplinas),
    % readCompromissos(ListaCompromissos),
    mainScreen(0),
    halt(0).