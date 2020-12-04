:-use_module(library(csv)).
:-include('Arquivos.pl').




%------------------------------VISUALIZACAO VENDA---------------------------------

mostraColunasVendas:-
    write('┌──────────────────Vendas──────────────────┐\n').
    % write('|     1.Cpf do funcionário | 2. Vendas.    |\n'),
    % write('────────────────────────────────────────────\n').
    
caracteristicasVenda(['││ CPF Funcionario: ', '││ ID Produto: ', '││ CPF Cliente: ', '││ Data: ']).

pegaCaracteristicaVenda(I, Caracteristica):-
    caracteristicasVenda(X), nth1(I, X, Caracteristica).
   

mostraListaVendasGeral([], _).
mostraListaVendasGeral([H|T], X):-
    Next is X + 1,
    pegaCaracteristicaVenda(X, Caracteristica),
    write(Caracteristica),
    write(H), write('\n'),
    mostraListaVendasGeral(T, Next).


mostraListaVendaGeral([],_).
mostraListaVendaGeral([H|T], X):-
    mostraListaVendasGeral(H, X),
    write('├──────────────────────────────────────────┤\n'),
    mostraListaVendaGeral(T,X).

mostraVendas(ProdutosVenda):-
    mostraColunasVendas,
    mostraListaVendaGeral(ProdutosVenda, 1).
