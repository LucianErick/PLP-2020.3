:-use_module(library(csv)).
:-include('Arquivos.pl').

cadastraProduto(Id, Nome, Preco, Validade) :-
    open('../arquivos/Produtos.csv', append, File),
    writeln(File, (Id, Nome, Preco, Validade)),
    close(File).

cadastraSintomaProduto(Id, Sintomas) :-
    open('../arquivos/SintomasProdutos.csv', append, File),
    sintomasEmLista(Sintomas, ListaSintomas),
    writeln(File, (Id, ListaSintomas)),
    close(File).

sintomasEmLista(StringSintomas, ListaSintomas) :-
    split_string(StringSintomas, '', ',', ListaSintomas).
%--------------------------------------------------------------------------------
mostraColunasDeEstoque:-
    write('------------------Produtos------------------\n'),
    write('1. Id, 2. Nome, 3. Preço, 4. Validade\n'),
    write('--------------------------------------------\n').
    
mostraListaProdutos([], _).
mostraListaProdutos([H|T], X):-
    Next is X + 1,
    write(X),
    write('. '),
    write(H), write('\n'),
    mostraListaProdutos(T, Next).


mostraLista([],_).
mostraLista([H|T], X):-
    mostraListaProdutos(H, X),
    write('--------------------------------------------\n'),
    mostraLista(T,X).

mostraProdutos(Produtos):-
    mostraColunasDeEstoque,
    mostraLista(Produtos, 1).

%--------------------------------------------------------------------------------

mostraColunasSintomas:-
    write('------------------SintomaProdutos------------------\n'),
    write('1. Id, 2. Sintomas\n'),
    write('--------------------------------------------\n').
    
mostraListaSintomaProduto([], _).
mostraListaSintomaProduto([H|T], X):-
    Next is X + 1,
    write(X),
    write('. '),
    write(H),
    mostraListaSintomaProduto(T, Next).


mostraListaSintoma([],_).
mostraListaSintoma([H|T], X):-
    mostraListaSintomaProduto(H, X),
    write('\n--------------------------------------------\n'),
    mostraListaSintoma(T,X).

mostraSintomaProduto(SintomaProduto):-
    mostraColunasSintomas,
    mostraListaSintoma(SintomaProduto, 1). 
=======
getProdutoPorId(_, [], false).
getProdutoPorId(IdProduto, [H|[]], P):- getProduto(IdProduto, H, P).
getProdutoPorId(IdProduto, [H|T], P):- getProdutoPorId(IdProduto, T, P).


getProduto(_, [], false).
getProduto(IdProduto, [IdProduto|Resto], [IdProduto|Resto]).
getProduto(IdProduto, [_|T], R):- getProduto(IdProduto, T, R).
