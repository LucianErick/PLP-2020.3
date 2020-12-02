:-use_module(library(csv)).
:-include('Arquivos.pl').

cadastraCliente(Cpf, Nome, DataCadastro) :-
    (clienteExiste(Cpf) -> false
    ;open('../arquivos/Clientes.csv', append, File),
    writeln(File, (Nome,Cpf,DataCadastro)),                 
    close(File)).


mostraColunas:-
    write('------------------Clientes------------------\n'),
    write('1.Nome, 2. Cpf, 3. Data Cadastro.\n'),
    write('--------------------------------------------\n').
    
mostraListaClientes([],_ , _).
mostraListaClientes([H|T], Cpf, X):-
    Next is X + 1,
    write(X),
    write('. '),
    write(H), write('\n'),
    (Next =:= 4 -> write('\nCompras do cliente:'), nl, mostraProdutoCliente(Cpf)
    ;write('')),
    mostraListaClientes(T, Cpf, Next).


mostraLista([],_).
mostraLista([H|T], X):-
    nth0(1, H, Cpf),
    mostraListaClientes(H, Cpf, X),
    write('--------------------------------------------\n'),
    mostraLista(T,X).

mostraClientes(Clientes):-
    mostraColunas,
    mostraLista(Clientes, 1).



clienteExiste(CpfCliente):-
    lerCsvRowList('Clientes.csv', Clientes),
    verificaClientes(CpfCliente, Clientes).

verificaClientes(_,[], false).
verificaClientes(SearchedCpf, [H|T]) :-
    (member(SearchedCpf, H) -> true
    ;verificaClientes(SearchedCpf, T)).


/* ----------------- cadastrar compras ---------------- */

adicionaCompra(IdCliente, IdProduto):-
    produtoExiste(IdProduto),
    clienteExiste(IdCliente),
    open('../arquivos/ComprasCliente.csv', append, File),
    writeln(File, (IdCliente, IdProduto)),
    close(File).
    

produtoExiste(IdProduto):-
    lerCsvRowList('Produtos.csv', Produtos),
    verificaProduto(IdProduto, Produtos).

verificaProduto(_,[], false).
verificaProduto(ProdutoId, [H|T]) :-
    (member(ProdutoId, H) -> true
    ;verificaClientes(ProdutoId, T)).


filtrarCompras(_, [], []).
filtrarCompras(IdCliente, [H|T], [X|D]):-
    removeIfNotPresent(IdCliente, H, X),
    filtrarCompras(IdCliente,T, D).


removeIfNotPresent(_, [], []).
removeIfNotPresent(Id, [Id,V|T], V).
removeIfNotPresent(Id, [H|T], []).

empty([]).

/* ------------------- mostrar compras ----------------- */

mostraProdutoCliente(Cpf):-
    getCompras(Cpf, Compras),
    exclude(empty,Compras,Produtos),
    length(Produtos,Length),
    (Length =:= 0 -> write('Sem produtos comprados!\n')
; write(Length), write(' produtos comprados!\n')).


getCompras(IdCliente, L):-
    lerCsvRowList("ComprasCliente.csv", Compras),
    filtrarCompras(IdCliente, Compras,R),
    exclude(empty, R, L).

