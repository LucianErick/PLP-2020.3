:-use_module(library(csv)).
:-include('Arquivos.pl').

cadastraCliente(Cpf, Nome, DataCadastro) :-
    open('../arquivos/Clientes.csv', append, File),
    writeln(File, (Nome,Cpf,DataCadastro)),                 %Só n sei como funciona o negocio de sitnomas e compras
    close(File).

mostraColunas:-
    write('------------------Clientes------------------\n'),
    write('1.Nome, 2. Cpf, 3. Data Cadastro, 4. Sintomas, 5. Compras.\n'),
    write('--------------------------------------------\n').
    
mostraListaClientes([], _).
mostraListaClientes([H|T], X):-
    Next is X + 1,
    write(X),
    write('. '),
    write(H), write('\n'),
    mostraListaClientes(T, Next).


mostraLista([],_).
mostraLista([H|T], X):-
    mostraListaClientes(H, X),
    write('--------------------------------------------\n'),
    mostraLista(T,X).

mostraClientes(Clientes):-
    mostraColunas,
    mostraLista(Clientes, 1).
