% :- :- initialization(main).


% FUNCOES UTEIS 
up(119).
down(115).
left(97).
right(100).
select(113).
remotion(101).

upAction(0, Limit, Limit).
upAction(Cursor, _, NewCursor) :- NewCursor is Cursor - 1.