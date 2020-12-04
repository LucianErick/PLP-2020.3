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
    removeIfNotPresentV(CpfFuncionario, H, X),
    filtrarVendas(CpfFuncionario,T, D).


removeIfNotPresentV(_, [], []).
removeIfNotPresentV(CpfFuncionario, [CpfFuncionario,V|T], [CpfFuncionario,V|T]).
removeIfNotPresentV(CpfFuncionario, [H|T], []).

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
    filtrarVendas(CpfFuncionario, Vendas,R),
    exclude(empty, R, L).


% -------------------- Visualização --------------------------

mostraColunasVendasFuncionario:-
    write('┌───────────────Visualização das Vendas────────────────┐\n').

caracteristicasVendasFuncionario(['││ CPF Funcionario: ', '││ ID Produto: ', '││ CPF CLiente: ', '││ Data: ']).

pegaCaracteristicaVendasFuncionario(I, Caracteristica):-
    caracteristicasVendasFuncionario(X), nth1(I, X, Caracteristica).


mostraListaVendas([], _).
mostraListaVendas([H|T], X):-
    Next is X + 1,
    pegaCaracteristicaVendasFuncionario(X, Caracteristica),
    write(Caracteristica),
    write(H), write('\n'),
    mostraListaVendas(T, Next).

mostraListaVenda([],_).
mostraListaVenda([H|T], X):-
    mostraListaVendas(H, X),
    write('├──────────────────────────────────────────────────────┤\n'),
    mostraListaVenda(T,X).

mostraVendasFuncionario(CpfFuncionario):-
    mostraColunasVendasFuncionario,
    getVendas(CpfFuncionario, Vendas),
    length(Vendas, Length),
    (Length = 0 -> write('│ Sem produtos vendidos!\n'),
    write('├──────────────────────────────────────────────────────┤\n');
    mostraListaVenda(Vendas, 1)).
    % mostraListaVenda(Vendas, 1).