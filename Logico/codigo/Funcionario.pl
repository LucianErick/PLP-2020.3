:-use_module(library(csv)).
:-include('Arquivos.pl').

cadastraFuncionario(Cpf, Nome, DataAdmissao, Salario) :-
    open('../arquivos/Funcionarios.csv', append, File),
    writeln(File, (Nome,Cpf,DataAdmissao,Salario)),                 %Só n sei como funciona o negocio de sitnomas e compras
    close(File).

mostraColunasFuncionario:-
    write('------------------Funcionarios--------------\n'),
    write('1.Nome, 2. Cpf, 3. Data Admissão, 4. Vendas, 5. Salário.\n'),
    write('--------------------------------------------\n').
    
mostraListaFuncionarios([], _).
mostraListaFuncionarios([H|T], X):-
    Next is X + 1,
    write(X),
    write('. '),
    write(H), write('\n'),
    mostraListaFuncionarios(T, Next).


mostraLista([],_).
mostraLista([H|T], X):-
    mostraListaFuncionarios(H, X),
    write('--------------------------------------------\n'),
    mostraLista(T,X).

mostraFuncionarios(Funcionarios):-
    mostraColunasFuncionario,
    mostraLista(Funcionarios, 1).