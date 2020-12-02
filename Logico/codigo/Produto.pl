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
mostraColunasProdutoEstoque:-
    write('------------------Produto-------------------\n'),
    write('1. ID, 2. Nome, 3. Data Validade, 4. Preço, 5. Sintomas.\n'),
    write('--------------------------------------------\n').
    
mostraListaProdutosEstoque([], _).
mostraListaProdutosEstoque([H|T], X):-
    Next is X + 1,
    write(X),
    write('. '),
    write(H), write('\n'),
    mostraListaProdutosEstoque(T, Next).

mostraListaEstoque([],_).
mostraListaEstoque([H|T], X):-
    mostraListaProdutosEstoque(H, X),
    write('--------------------------------------------\n'),
    mostraListaEstoque(T,X).

mostraProdutosEstoque(Estoque):-
    mostraColunasProdutoEstoque,
    mostraListaEstoque(Estoque, 1).
%------------------------------------------------------------------------------------

mostraColunasProdutoSintomas:-
    write('------------------Produto-------------------\n'),
    write('1. ID, 2. Sintomas\n'),
    write('--------------------------------------------\n').

mostraListaProdutosSintomas([], _).
mostraListaProdutosSintomas([H|T], X):-
    write(X),
    write('. '),
    write(H), write('\n'),
    mostraListaProdutosSintomas(T, Next).

mostraListaSintomas([],_).
mostraListaSintomas([H|T], X):-
    mostraListaProdutosSintomas(H, X),
    write('--------------------------------------------\n'),
    mostraListaSintomas(T,X).

mostraProdutosSintomas(Sintomas):-
    mostraColunasProdutoSintomas,
    mostraListaSintomas(Sintomas, 1).
