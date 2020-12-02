:-use_module(library(csv)).
:-include('Arquivos.pl').
:-include('Cliente.pl').

%--------------------------CADASTRO FUNCIONARIO--------------------------------

cadastraFuncionario(Cpf, Nome, DataAdmissao, Salario) :-
    funcionarioExiste(Cpf),
    open('../arquivos/Funcionarios.csv', append, File),
    writeln(File, (Nome,Cpf,DataAdmissao,Salario)),                 
    close(File).

%------------------------------VISUALIZACAO FUNCIONARIO---------------------------------

mostraColunasFuncionario:-
    write('------------------Funcionarios--------------\n'),
    write('1.Nome, 2. Cpf, 3. Data Admissão, 4. Salário, 5. Vendas.\n'),
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

%------------------------------VISUALIZACAO VENDA---------------------------------

mostraColunasVendas:-
    write('------------------Vendas--------------\n'),
    write('1.Cpf do funcionário, 2. Vendas.\n'),
    write('--------------------------------------------\n').
    
mostraListaVendas([], _).
mostraListaVendas([H|T], X):-
    Next is X + 1,
    write(X),
    write('. '),
    write(H), write('\n'),
    mostraListaVendas(T, Next).


mostraLista([],_).
mostraLista([H|T], X):-
    mostraListaVendas(H, X),
    write('--------------------------------------------\n'),
    mostraLista(T,X).

mostraFuncionarios(ProdutosVenda):-
    mostraColunasVendas,
    mostraLista(ProdutosVenda, 1).

%-----------------------------CADASTRO VENDA----------------------------------

cadastraVenda(CpfFuncionario, IdProduto, CpfCliente):-
    produtoExiste(IdProduto),
    clienteExiste(CpfCliente),
    funcionarioExiste(CpfFuncionario),
    open('../arquivos/ProdutosVenda.csv', append, File),
    writeln(File, (CpfFuncionario, IdProduto, CpfCliente)),                 
    close(File).

filtrarVendas(_, [], []).
filtrarVendas(CpfFuncionario, [H|T], [X|D]):-
    removeIfNotPresent(CpfFuncionario, H, X),
    filtrarVendas(CpfFuncionario,T, D).


removeIfNotPresent(_, [], []).
removeIfNotPresent(Cpf, [Cpf,V|T], V).
removeIfNotPresent(Cpf, [H|T], []).

empty([]).
    
%-----------------------------VALIDA PRODUTO EXISTE--------------------------------

produtoExiste(IdProduto):-
    lerCsvRowList('Produtos.csv', Produtos),
    verificaProduto(IdProduto, Produtos).

verificaProduto(_,[], false).
verificaProduto(ProdutoId, [H|T]) :-
    (member(ProdutoId, H) -> true
    ;verificaProduto(ProdutoId, T)).

%----------------------MOSTRAR QTD VENDAS DO FUNCIONARIO------------------------

mostraVendaFuncionario(Cpf):-
    getVendas(Cpf, Vendas),
    exclude(empty,Vendas,Produtos),
    length(Produtos,Length),
    (Length =:= 0 -> write('Sem produtos vendidos!\n')
; write(Length), write(' produtos vendidos!\n')).


getCompras(IdCliente, L):-
    lerCsvRowList("ProdutosVenda.csv", Vendas),
    filtrarVendas(CpfFuncionario, Vendas,R),
    exclude(empty, R, L).

%-----------------------------VALIDAFUNCIONARIOEXISTE------------------------------

funcionarioExiste(CpfFuncionario):-
    lerCsvRowList('../arquivos/Funcinarios.csv', Funcinarios),
    verificaFuncionarios(CpfFuncionario, Funcinarios).

verificaFuncionarios(_,[], false).
verificaFuncionarios(SearchedCpf, [H|T]) :-
    (member(SearchedCpf, H) -> true
    ;verificaFuncionarios(SearchedCpf, T)).

%-----------------------------------LEITURAFUNCIONARIO----------------------------------------

ler_arquivo(Result) :-
    open('../arquivos/Funcionarios.csv',read,Str),
    read_stream_to_codes(Str,Funcionarios),
    atom_string(Funcionarios,Funcionarios1),
    split_string(Funcionarios1,"\n","",Result).

%-------------------------------------LEITURAVENDAS---------------------------------------------

ler_arquivo(Result) :-
    open('../arquivos/ProdutosVenda.csv',read,Str),
    read_stream_to_codes(Str,ProdutosVenda),
    atom_string(ProdutosVenda,ProdutosVenda1),
    split_string(ProdutosVenda1,"\n","",Result).