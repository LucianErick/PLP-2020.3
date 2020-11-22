% :- :- initialization(main).


/*------------------------------------------FUNCOES UTEIS----------------------------------------------*/
up(119).
down(115).
left(97).
right(100).
select(113).
remotion(101).

upAction(0, Limit, Limit).
upAction(Cursor, _, NewCursor) :- NewCursor is Cursor - 1.

downAction(Cursor, Limit, NewCursor) :- Max is Limit + 1,
                                        Pc is Cursor + 1,
                                        NewCursor is PC mod Max.

% switch([O|Os], Item, Pos, Cont, NewO) :-
%     (Pos =:= Cont -> NewO = [Item|Os];
%         Cont2 is Cont + 1, switch(Os, Item, Pos, Cont2, NewO2), NewO = [O|NewO2]).

% remove([], _, _, []).
% remove([O|Os], Pos, Cont, NewO) :-
%     (Pos =:= Cont -> NewO = Os;
%         Cont2 is Cont + 1, remove(Os, Pos, Cont2, NewO2), NewO = [O|NewO2]).

% add([], Item, Item).
% add([O|Os], Item, NewO) :-
%     add(Os, Item, NewO2),
%     NewO = [O|NewO2].

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

listNomesDisciplinas([], []).
listNomesDisciplinas([[Nome|Resto]|Ds], ListaOpcoes) :-
                    listNomesDisciplinas(Ds, ListaOpcoes2),
                    ListaOpcoes = [Nome|ListaOpcoes2].
/*------------------------------------------FUNCOES PARA IO----------------------------------------------*/

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


/*------------------------------------------TELA PRINCIPAL----------------------------------------------*/

optionsMainScreen(['Entrar como gestor', 'Entrar como funcionário', 'Entrar como cliente', 'Sair do sistema']).
limitMain(4).

/*-------------------------------------------------------------------------------------------------------*/

showExitMessage() :-
    shell(clear),
    writeln('---------------------------------------------'),
    writeln('* OBRIGADO POR UTILIZAR O CORONA PHARM *'),
    writeln('*                 VOLTE SEMPRE!                 *'),
    writeln('---------------------------------------------\n').

doMainScreen(Cursor, Action) :-
    limitMain(Limit),
    (up(Action) -> upAction(Cursor, Limit, NewCursor), mainScreen(NewCursor);
     down(Action) -> downAction(Cursor, Limit, NewCursor), mainScreen(NewCursor);
     left(Action) -> showExitMessage();
     right(Action) -> (Cursor =:= 0 -> write('Você é um gestor.');
                       Cursor =:= 1 -> write('Você é um funcionário.');
                       Cursor =:= 2 -> write('Você é um cliente.');
                       Cursor =:= 3 -> write('Você quer sair do sistema.'));
     mainScreen(Cursor)).

mainScreen(Cursor) :-
    shell(clear),
    writeln('\n|| (w,s) Para mover o cursor  ||'),
    writeln('|| (a) Para modificar a tela  ||'),
    writeln('|| (d) Para entrar nas opcoes ||\n'),
    optionsMainScreen(ListaOpcoes),
    showOptions(ListaOpcoes, Cursor, 0),
    get_single_char(Action),
    doMainScreen(Cursor, Action).

/*--------------------------------------------TELA GESTOR-----------------------------------------------------------*/

% optionsConfiguracoesScreen(['Cadastrar disciplina', 'Atualizar disciplina', 'Remover disciplina', 'Resetar sistema']).
% limitGestor(3).
