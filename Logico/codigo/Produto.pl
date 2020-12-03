:-use_module(library(csv)).
:-include('Arquivos.pl').

cadastraProduto(Id, Nome, Preco, Validade) :-
    open('../arquivos/Produtos.csv', append, File),
    writeln(File, (Id, Nome, Preco, Validade)),
    close(File).

cadastraSintomaProduto(Id, Sintomas) :-
    open('../arquivos/SintomasProdutos.csv', append, File),
    writeln(File, (Id, Sintomas)),
    close(File).

sintomasEmLista(StringSintomas, ListaSintomas) :-
    split_string(StringSintomas, '', ',', ListaSintomas).
%--------------------------------------------------------------------------------
mostraColunasDeEstoque:-
    write('┌───────────────────────Produtos───────────────────────┐\n').
    % write('|        1. Id, 2. Nome, 3. Preço, 4. Validade         |\n'),
    % write('────────────────────────────────────────────────────────\n').

caracteristicasProduto(['││ Id: ', '││ Nome: ', '││ Preço: ', '││ Validade: ']).

pegaCaracteristicaProduto(I, Caracteristica):-
    caracteristicasProduto(X), nth1(I, X, Caracteristica).
   
mostraListaProdutos([], _).
mostraListaProdutos([H|T], X):-
    Next is X + 1,
    pegaCaracteristicaProduto(X, Caracteristica),
    write(Caracteristica),
    write(H), write('\n'),
    mostraListaProdutos(T, Next).


mostraListaProd([],_).
mostraListaProd([H|T], X):-
    mostraListaProdutos(H, X),
    write('├──────────────────────────────────────────────────────┤\n'),
    mostraListaProd(T,X).

mostraProdutos(Produtos):-
    mostraColunasDeEstoque,
    mostraListaProd(Produtos, 1).

%--------------------------------------------------------------------------------

mostraColunasSintomas:-
    write('┌─────────────────Visualização por Sintoma─────────────────┐\n').
    % write('|                    1. Id, 2. Sintomas                    |\n'),
    % write('────────────────────────────────────────────────────────────\n').
    
caracteristicasProduto2(['││ Id: ', '││ Sintomas: ']).

pegaCaracteristica2(I, Caracteristica):-
    caracteristicasProduto2(X), nth1(I, X, Caracteristica).

mostraListaSintomaProduto([], _).
mostraListaSintomaProduto([H|T], X):-
    Next is X + 1,
    pegaCaracteristica2(X, Caracteristica),
    write(Caracteristica),
    write(H), write('\n'),
    mostraListaSintomaProduto(T, Next).


mostraListaSintoma([],_).
mostraListaSintoma([H|T], X):-
    mostraListaSintomaProduto(H, X),
    write('├──────────────────────────────────────────────────────────┤\n'),
    mostraListaSintoma(T,X).

mostraSintomaProduto(SintomaProduto):-
    mostraColunasSintomas,
    mostraListaSintoma(SintomaProduto, 1).

getProdutoPorId(_, [], false).
getProdutoPorId(IdProduto, [H|[]], P):- getProduto(IdProduto, H, P).
getProdutoPorId(IdProduto, [H|T], P):- getProdutoPorId(IdProduto, T, P).

getProduto(_, [], false).
getProduto(IdProduto, [IdProduto|Resto], [IdProduto|Resto]).
getProduto(IdProduto, [_|T], R):- getProduto(IdProduto, T, R).