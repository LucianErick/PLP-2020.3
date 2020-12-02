:-use_module(library(csv)).
:-include('Arquivos.pl').
:-include('Cliente.pl').


%----------------------------CADASTRO-----------------------------------

cadastraFuncionario(Cpf, Nome, DataAdmissao, Salario) :-
    (funcionarioExiste(Cpf) -> false
    ;open('../arquivos/Funcionarios.csv', append, File),
    writeln(File, (Nome,Cpf,DataAdmissao, Salario)),                 
    close(File)).

%---------------------------VISUALIZACAO----------------------------------
mostraColunasFuncionarios:-
    write('------------------Funcionarios------------------\n'),
    write('1.Nome, 2. Cpf, 3. Data Admissão, 4.Salário, 5.QTD de Vendas.\n'),
    write('--------------------------------------------\n').
    
mostraListaFuncionarios([],_ , _).
mostraListaFuncionarios([H|T], CpfFuncionario, X):-
    Next is X + 1,
    write(X),
    write('. '),
    write(H), write('\n'),
    (X =:= 4 -> write('\nVendas do funcionários:'), nl, mostraQTDVendasFuncionario(CpfFuncionario)
    ;write('')),
    mostraListaFuncionarios(T, CpfFuncionario, Next).


mostraArray([],_).
mostraArray([H|T], X):-
    nth0(1, H, CpfFuncionario),
    mostraListaFuncionarios(H, CpfFuncionario, X),
    write('--------------------------------------------\n'),
    mostraArray(T,X).

mostraFuncionarios(Funcionarios):-
    mostraColunasFuncionarios,
    mostraArray(Funcionarios, 1).


funcionarioExiste(CpfFuncionario):-
    lerCsvRowList('Funcionarios.csv', Funcionarios),
    verificaFuncionarios(CpfFuncionario, Funcionarios).

verificaFuncionarios(_,[], false).
verificaFuncionarios(SearchedCpf, [H|T]) :-
    (member(SearchedCpf, H) -> true
    ;verificaFuncionarios(SearchedCpf, T)).

%--------------------------------VENDAS--------------------------------------

adicionaVenda(CpfFuncionario, IdProduto, IdCliente, DataVenda):-
    produtoExiste(IdProduto),
    clienteExiste(IdCliente),
    funcionarioExiste(CpfFuncionario),
    open('../arquivos/ComprasCliente.csv', append, File),
    writeln(File, (CpfFuncionario, IdProduto, IdCliente, DataVenda)),
    close(File).
    
%--------------------------VERIFICA CLIENTE/PRODUTO----------------------------

produtoExiste(IdProduto):-
    lerCsvRowList('Produtos.csv', Produtos),
    verificaProduto(IdProduto, Produtos).

verificaProduto(_,[], false).
verificaProduto(ProdutoId, [H|T]) :-
    (member(ProdutoId, H) -> true
    ;verificaClientes(ProdutoId, T)).

clienteExiste(CpfCliente):-
    lerCsvRowList('Clientes.csv', Clientes),
    verificaClientes(CpfCliente, Clientes).

verificaClientes(_,[], false).
verificaClientes(SearchedCpf, [H|T]) :-
    (member(SearchedCpf, H) -> true
    ;verificaClientes(SearchedCpf, T)).

%----------------------------------FILTRO------------------------------------------

filtrarVendas(_, [], []).
filtrarVendas(CpfFuncionario, [H|T], [X|D]):-
    removeIfNotPresent(CpfFuncionario, H, X),
    filtrarVendas(CpfFuncionario,T, D).


removeIfNotPresent(_, [], []).
removeIfNotPresent(CpfFuncionario, [CpfFuncionario,V|T], V).
removeIfNotPresent(CpfFuncionario, [H|T], []).

empty([]).

%---------------------MOSTRA-QTD-VENDAS-------------------------------

mostraQTDVendasFuncionario(CpfFuncionario):-
    getVendas(CpfFuncionario, Vendas),
    exclude(empty,Vendas,Produtos),
    length(Produtos,Length),
    (Length =:= 0 -> write('Sem produtos vendidos!\n')
; write(Length), write(' produtos vendidos!\n')).


getVendas(CpfFuncionario, L):-
    lerCsvRowList("ProdutosVendas.csv", Vendas),
    filtrarCompras(CpfFuncionario, Vendas,R),
    exclude(empty, R, L).

