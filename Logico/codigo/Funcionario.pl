:-use_module(library(csv)).
:-include('Arquivos.pl').

------------------------------CADASTRO-------------------------------------

cadastraFuncionario(Cpf, Nome, DataAdmissao, Salario) :-
    open('../arquivos/Funcionarios.csv', append, File),
    writeln(File, (Nome,Cpf,DataAdmissao,Salario)),                 %Só n sei como funciona o negocio de sintomas e compras
    close(File).

------------------------------VIEW ENTRADA---------------------------------

mostraColunasFuncionario:-
    write('------------------Funcionarios--------------\n'),
    write('1.Nome, 2. Cpf, 3. Data Admissão, 4. Vendas, 5. Salário.\n'),
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


--------------------------------ATUALIZA VENDAS--------------------------------------

atualiza([],_,_,[]).
atualiza([X|Xs],String,NovaVenda,[NovaVenda1|Xs]) :-
    split_string(X,"-","",X1),
    target(X1,String),
    formata_venda(NovaVenda1,NovaVenda),!.
atualiza([X|Xs],String,NovaVenda,[X|Resultado]) :- atualiza(Xs,String,NovaVenda,Resultado).

atualiza_vendas(String,NovaVendas) :-
    ler_arquivo(Funcionarios),
    atualiza(Funcionarios,String,NovaVendas,Atualizada),
    atomic_list_concat(Atualizado,"\n",S),
    open('../arquivos/Funcionarios.csv',write,File),
    write(File,S),
    close(File).


-------------------------------------LEITURA---------------------------------------------

ler_arquivo(Result) :-
    open('../arquivos/Funcionarios.csv',read,Str),
    read_stream_to_codes(Str,Funcionarios),
    atom_string(Funcionarios,Funcionarios1),
    split_string(Funcionarios1,"\n","",Result).