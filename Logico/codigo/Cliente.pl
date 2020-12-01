:-use_module(library(csv)).
:-include('Arquivos.pl').

cadastraCliente(Cpf, Nome, DataCadastro) :-
    clienteExiste(Cpf),
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



clienteExiste(CpfCliente):-
    lerCsvRowList('Clientes.csv', Clientes),
    verificaClientes(CpfCliente, Clientes).

verificaClientes(_,[], false).
verificaClientes(SearchedCpf, [H|T]) :-
    (member(SearchedCpf, H) -> true
    ;verificaClientes(SearchedCpf, T)).


/* ----------------- compras ---------------- */

adicionaCompra(IdCliente, IdProduto):-
    produtoExiste(IdProduto),
    clienteExiste(IdCliente),
    open('../arquivos/ComprasCliente.csv', append, File),
    writeln(File, (IdCliente, IdProduto)),                 %Só n sei como funciona o negocio de sitnomas e compras
    close(File).
    

produtoExiste(IdProduto):-
    lerCsvRowList('Produtos.csv', Produtos),
    verificaProduto(IdProduto, Produtos).

verificaProduto(_,[], false).
verificaProduto(ProdutoId, [H|T]) :-
    (member(ProdutoId, H) -> true
    ;verificaClientes(ProdutoId, T)).