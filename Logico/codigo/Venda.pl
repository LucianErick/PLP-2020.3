:-use_module(library(csv)).
:-include('Arquivos.pl').




%------------------------------VISUALIZACAO VENDA---------------------------------

mostraColunasVendas:-
    write('───────────────────Vendas───────────────────\n').
    % write('|     1.Cpf do funcionário | 2. Vendas.    |\n'),
    % write('────────────────────────────────────────────\n').
    
caracteristicas(['| CPF do funcionário: ', '| Vendas: ']).

pegaCaracteristica(I, Caracteristica):-
    caracteristicas(X), nth1(I, X, Caracteristica).
   

mostraListaVendas([], _).
mostraListaVendas([H|T], X):-
    Next is X + 1,
    pegaCaracteristica(X, Caracteristica),
    write(Caracteristica),
    write(H), write('\n'),
    mostraListaVendas(T, Next).


mostraLista([],_).
mostraLista([H|T], X):-
    mostraListaVendas(H, X),
    write('────────────────────────────────────────────\n'),
    mostraLista(T,X).

mostraVendas(ProdutosVenda):-
    mostraColunasVendas,
    mostraLista(ProdutosVenda, 1).
    