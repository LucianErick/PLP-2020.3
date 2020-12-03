:-use_module(library(csv)).
:-include('Arquivos.pl').
:-include('Cliente.pl').


%----------------------------CADASTRO-----------------------------------

cadastraFuncionario(Cpf, Nome, DataAdmissao, Salario) :-
    open('../arquivos/Funcionarios.csv', append, File),
    writeln(File, (Nome,Cpf,DataAdmissao, Salario)),                 
    close(File).

%---------------------------VISUALIZACAO----------------------------------
mostraColunasFuncionarios:-
    write('┌────────────────────────Funcionarios──────────────────────────┐\n').
    % write('|1.Nome, 2. Cpf, 3. Data Admissão, 4.Salário, 5.QTD de Vendas. |\n'),
    % write('────────────────────────────────────────────────────────────────\n').
    
caracteristicasFuncionarios(['││ Nome: ', '││ CPF: ', '││ Data Admissão: ', '││ Salário: ', '││ QTD de Vendas: ']).

pegaCaracteristicaFuncionario(I, Caracteristica):-
    caracteristicasFuncionarios(X), nth1(I, X, Caracteristica).

mostraListaFuncionarios([],_ , _).
mostraListaFuncionarios([H|T], CpfFuncionario, X):-
    Next is X + 1,
    pegaCaracteristicaFuncionario(X, Caracteristica),
    write(Caracteristica),
    write(H), write('\n'),
    (X =:= 4 -> write('│\n│ Vendas do funcionários:'), nl, mostraQTDVendasFuncionario(CpfFuncionario)
    ;write('')),
    mostraListaFuncionarios(T, CpfFuncionario, Next).


mostraArray([],_).
mostraArray([H|T], X):-
    nth0(1, H, CpfFuncionario),
    mostraListaFuncionarios(H, CpfFuncionario, X),
    write('├──────────────────────────────────────────────────────────────┤\n'),
    mostraArray(T,X).

mostraFuncionarios(Funcionarios):-
    mostraColunasFuncionarios,
    mostraArray(Funcionarios, 1).


%--------------------------------VENDAS--------------------------------------

adicionaVenda(CpfFuncionario, IdProduto, IdCliente, DataVenda):-
    open('../arquivos/ProdutosVenda.csv', append, File),
    writeln(File, (CpfFuncionario, IdProduto, IdCliente, DataVenda)),
    close(File).

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
    (Length =:= 0 -> write('│ Sem produtos vendidos!\n')
; write('│ '), write(Length), write(' produtos vendidos!\n')).


getVendas(CpfFuncionario, L):-
    lerCsvRowList('ProdutosVenda.csv', Vendas),
    filtrarCompras(CpfFuncionario, Vendas,R),
    exclude(empty, R, L).

