:-use_module(library(csv)).
:-include('Arquivos.pl').

------------------------------CADASTRO-------------------------------------

cadastraFuncionario(Cpf, Nome, DataAdmissao, Salario) :-
    funcionarioExiste(Cpf)
    open('../arquivos/Funcionarios.csv', append, File),
    writeln(File, (Nome,Cpf,DataAdmissao,Salario)),                 
    close(File).

------------------------------VIEWENTRADA---------------------------------

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


--------------------------------ADICIONAVENDAS--------------------------------------


adicionaCompra(CpfFuncionario, IdProduto):-
    produtoExiste(IdProduto),
    funcionarioExiste(CpfFuncionario),
    open('../arquivos/ProdutosVenda.csv', append, File),
    writeln(File, (CpfFuncionario, IdProduto)),                 
    close(File).
    

produtoExiste(IdProduto):-
    lerCsvRowList('Produtos.csv', Produtos),
    verificaProduto(IdProduto, Produtos).

verificaProduto(_,[], false).
verificaProduto(ProdutoId, [H|T]) :-
    (member(ProdutoId, H) -> true
    ;verificaProduto(ProdutoId, T)).


----------------------------------VALIDAFUNCIONARIOEXISTE------------------------------------------

funcionarioExiste(CpfFuncionario):-
    lerCsvRowList('../arquivos/Funcinarios.csv', Funcinarios),
    verificaFuncionarios(CpfFuncionario, Funcinarios).

verificaFuncionarios(_,[], false).
verificaFuncionarios(SearchedCpf, [H|T]) :-
    (member(SearchedCpf, H) -> true
    ;verificaFuncionarios(SearchedCpf, T)).


-------------------------------------LEITURA---------------------------------------------

ler_arquivo(Result) :-
    open('../arquivos/Funcionarios.csv',read,Str),
    read_stream_to_codes(Str,Funcionarios),
    atom_string(Funcionarios,Funcionarios1),
    split_string(Funcionarios1,"\n","",Result).