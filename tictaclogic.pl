size(6).

fixed_cell(1, 3, x).

fixed_cell(2, 3, x).

fixed_cell(3, 1, x).
fixed_cell(3, 6, x).

fixed_cell(4, 3, o).

fixed_cell(5, 2, x).
fixed_cell(5, 6, x).

fixed_cell(6, 1, o).
fixed_cell(6, 5, o).

cell(X, Y, S) :- fixed_cell(X,Y,S), !; solve_cell(X,Y,S), !.
cell(X, Y, e) :- \+ fixed_cell(X, Y, _), \+ solve_cell(X, Y, _).

get_row(Row, R) :- size(I), get_row_helper(Row, I, R).
get_row_helper(Row, I, R) :- I > 0, cell(Row, I, Letter), NewI is I - 1, get_row_helper(Row, NewI, Rest), append(Rest, [Letter], R), !.
get_row_helper(_, 0, []).

get_all_rows(R) :- size(I), get_all_rows_helper(I, R).
get_all_rows_helper(I, [Row|Rest]) :- I > 0, get_row(I, Row), NewI is I - 1, get_all_rows_helper(NewI, Rest).
get_all_rows_helper(0, []).

get_col(Col, R) :- size(I), get_col_helper(Col, I, R).
get_col_helper(Col, I, R) :- I > 0, cell(I, Col, Letter), NewI is I - 1, get_col_helper(Col, NewI, Rest), append(Rest, [Letter], R), !.
get_col_helper(_, 0, []).

get_all_cols(R) :- size(I), get_all_cols_helper(I, R).
get_all_cols_helper(I, [Col|Rest]) :- I > 0, get_col(I, Col), NewI is I - 1, get_all_cols_helper(NewI, Rest).
get_all_cols_helper(0, []).

get_all_rows_and_cols(R) :- get_all_rows(Rows), get_all_cols(Cols), append(Rows, Cols, R).

count_symbol_in_list(Symbol, List, R) :- I is 0, count_symbol_in_list_helper(Symbol, List, I, R).
count_symbol_in_list_helper(Symbol, [H|T], I, R) :- (Symbol == H, NewI is I + 1; NewI is I), count_symbol_in_list_helper(Symbol, T, NewI, R), !.
count_symbol_in_list_helper(_, [], I, I).

are_lists_equal(List1, List2) :- List1 == List2.

print_board :-
    size(N),
    print_rows(1, N), !. 

print_rows(Row, N) :-
    Row =< N,
    print_vertical_border,
    print_cells(Row, 1, N),
    nl,
    Row1 is Row + 1,
    print_rows(Row1, N).
print_rows(_, _).

print_cells(Row, Col, N) :-
    Col =< N,
    (cell(Row, Col, S), !),
    write(' '),
    write_cell(S),
    write(' |'),
    Col1 is Col + 1,
    print_cells(Row, Col1, N).
print_cells(Row, Col, N) :-
    Col =< N,
    \+ cell(Row, Col, _),
    write(' '),
    write_cell(-),
    write(' |'),
    Col1 is Col + 1,
    print_cells(Row, Col1, N).
print_cells(_, _, _).

print_vertical_border :-
    write('|').

write_cell(S) :-
    write(' '),
    (S \= e, write(S); write('-')),
    write(' ').

%====
all_cells_filled :- size(I), all_cells_filled_helper(I).
all_cells_filled_helper(I) :- I > 0, get_row(I, Row), all_cells_filled_in_list(Row), NewI is I - 1, all_cells_filled_helper(NewI), !.
all_cells_filled_helper(0).
all_cells_filled_in_list([H|T]) :- H \= e, all_cells_filled_in_list(T).
all_cells_filled_in_list([]).

no_tripple :- size(I), no_tripple_helper(I).
no_tripple_helper(I) :- no_tripple_helper_cols(I), no_tripple_helper_rows(I).
no_tripple_helper_cols(I) :- I > 0, get_col(I, Col), \+ tripple_in_list(Col, x), \+ tripple_in_list(Col, o), NewI is I - 1, no_tripple_helper_cols(NewI).
no_tripple_helper_cols(0).
no_tripple_helper_rows(I) :- I > 0, get_row(I, Row), \+ tripple_in_list(Row, x), \+ tripple_in_list(Row, o), NewI is I - 1, no_tripple_helper_rows(NewI).
no_tripple_helper_rows(0).
tripple_in_list([Symbol, Symbol, Symbol|_], Symbol) :- !.
tripple_in_list([Fst, Snd, Trd|T], Symbol) :- (Fst \= Symbol; Snd \= Symbol; Trd \= Symbol), tripple_in_list([Snd, Trd|T], Symbol).

symbol_count_correct :- size(I), Count is I / 2, symbol_count_correct_helper(I, Count).
symbol_count_correct_helper(I, Count) :- I > 0,
    get_row(I, Row), count_symbol_in_list(x, Row, XsInRow), XsInRow == Count,
    get_row(I, Row), count_symbol_in_list(o, Row, OsInRow), OsInRow == Count,
    get_col(I, Col), count_symbol_in_list(x, Col, XsInCol), XsInCol == Count,
    get_col(I, Col), count_symbol_in_list(o, Col, OsInCol), OsInCol == Count,
    NewI is I - 1, symbol_count_correct_helper(NewI, Count).
symbol_count_correct_helper(0, _).

no_repeat :- get_all_rows(ListOfRows), get_all_cols(ListOfCols), no_repeat_helper(ListOfRows), no_repeat_helper(ListOfCols).
no_repeat_helper([H|T]) :- \+ member(H, T), no_repeat_helper(T).
no_repeat_helper([]).


solved :-
   all_cells_filled,
   no_tripple,
   symbol_count_correct,
   no_repeat.

%==== ////////////////////////////////////////////////////////////
:- dynamic solve_cell/3.

set_solve_cell(X, Y, Symbol) :- size(S), (X > 0, X =< S, Y > 0, Y =< S, assertz(solve_cell(X, Y, Symbol)); true), !.

clear :- retractall(solve_cell(_, _, _)).

opposite_symbol(x, o).
opposite_symbol(o, x).

avoiding_triples_1 :- 
    size(I), 
    (avoiding_triples_1_row_helper(I, 1); true), 
    avoiding_triples_1_col_helper(I, 1), !.
avoiding_triples_1_row_helper(I, J) :- 
    I > 0, size(S), \+ (J > S),
    J2 is J + 1, L is J - 1, R is J + 2,
    cell(I, J, C1), cell(I, J2, C2),
    (C1 == C2, C1 \= e, opposite_symbol(C1, OpSym), 
    (cell(I, L, LS), LS == e, set_solve_cell(I, L, OpSym); true), 
    (cell(I, R, RS), RS == e, set_solve_cell(I, R, OpSym); true); 
    true),
    NewJ is J + 1, avoiding_triples_1_row_helper(I, NewJ), !.
avoiding_triples_1_row_helper(I, J) :- 
    I > 0, size(S), J > S, NewI is I - 1, 
    avoiding_triples_1_row_helper(NewI, 1), !.
avoiding_triples_1_row_helper(0, _).
avoiding_triples_1_col_helper(I, J) :- 
    I > 0, size(S), \+ (J > S),
    J2 is J + 1, L is J - 1, R is J + 2,
    cell(J, I, C1), cell(J2, I, C2),
    (C1 == C2, C1 \= e, opposite_symbol(C1, OpSym), 
    (cell(L, I, LS), LS == e, set_solve_cell(L, I, OpSym); true), 
    (cell(R, I, RS), RS == e, set_solve_cell(R, I, OpSym); true); 
    true),
    NewJ is J + 1, avoiding_triples_1_col_helper(I, NewJ), !.
avoiding_triples_1_col_helper(I, J) :- 
    I > 0, size(S), J > S, NewI is I - 1, 
    avoiding_triples_1_col_helper(NewI, 1), !.
avoiding_triples_1_col_helper(0, _).

avoiding_triples_2 :- 
    size(I), 
    (avoiding_triples_2_row_helper(I, 1); true), 
    avoiding_triples_2_col_helper(I, 1), !.
avoiding_triples_2_row_helper(I, J) :- 
    I > 0, size(S), \+ (J > S),
    J2 is J + 1, J3 is J + 2,
    cell(I, J, C1), cell(I, J2, C2), cell(I, J3, C3),
    (C1 == C3, C1 \= e, C2 == e, opposite_symbol(C1, OpSym), 
    set_solve_cell(I, J2, OpSym); true),
    NewJ is J + 1, avoiding_triples_2_row_helper(I, NewJ), !.
avoiding_triples_2_row_helper(I, J) :- 
    I > 0, size(S), J > S, NewI is I - 1, 
    avoiding_triples_2_row_helper(NewI, 1), !.
avoiding_triples_2_row_helper(0, _).
avoiding_triples_2_col_helper(I, J) :- 
    I > 0, size(S), \+ (J > S),
    J2 is J + 1, J3 is J + 2,
    cell(J, I, C1), cell(J2, I, C2), cell(J3, I, C3),
    (C1 == C3, C1 \= e, C2 == e, opposite_symbol(C1, OpSym), 
    set_solve_cell(J2, I, OpSym); true),
    NewJ is J + 1, avoiding_triples_2_col_helper(I, NewJ), !.
avoiding_triples_2_col_helper(I, J) :- 
    I > 0, size(S), J > S, NewI is I - 1, 
    avoiding_triples_2_col_helper(NewI, 1), !.
avoiding_triples_2_col_helper(0, _).

complete_rows :- size(I), (complete_rows_helper(I); true), !.
complete_rows_helper(I) :- 
    I > 0, size(S), Count is S / 2, PCount is Count - 1, get_row(I, Row), 
    count_symbol_in_list(x, Row, C1), count_symbol_in_list(o, Row, C2),
    ((C1 == Count, C2 == PCount, place_sym_in_e_row(I, o));
    (C2 == Count, C1 == PCount, place_sym_in_e_row(I, x)); true),
    NewI is I - 1, complete_rows_helper(NewI). 
    complete_rows_helper(0).
place_sym_in_e_row(I, Symbol) :- place_sym_in_e_row_helper(I, 1, Symbol).
place_sym_in_e_row_helper(I, J, Symbol) :- 
    size(S), J =< S,
    cell(I, J, C), 
    (C == e, set_solve_cell(I, J, Symbol), !; true),
    NewJ is J + 1, place_sym_in_e_row_helper(I, NewJ, Symbol).
place_sym_in_e_row_helper(_, J, _) :- size(S), J > S, !.

complete_cols :- size(I), (complete_cols_helper(I); true), !.
complete_cols_helper(I) :- 
    I > 0, size(S), Count is S / 2, PCount is Count - 1, get_col(I, Col), 
    count_symbol_in_list(x, Col, C1), count_symbol_in_list(o, Col, C2),
    ((C1 == Count, C2 == PCount, place_sym_in_e_col(I, o));
    (C2 == Count, C1 == PCount, place_sym_in_e_col(I, x)); true),
    NewI is I - 1, complete_cols_helper(NewI), !. 
    complete_cols_helper(0).
place_sym_in_e_col(I, Symbol) :- place_sym_in_e_col_helper(I, 1, Symbol).
place_sym_in_e_col_helper(I, J, Symbol) :- 
    size(S), J =< S,
    cell(J, I, C), 
    (C == e, set_solve_cell(J, I, Symbol), !; true),
    NewJ is J + 1, place_sym_in_e_col_helper(I, NewJ, Symbol).
place_sym_in_e_col_helper(_, J, _) :- size(S), J > S, !.

avoiding_triples_3.% NOT IMPLEMENTED

avoid_rows_dup.% NOT IMPLEMENTED

avoid_cols_dup.% NOT IMPLEMETED

solve_cycler :- 
    nl,
    print_board,
    nl,
    get_all_rows_and_cols(B1),
    avoiding_triples_1, avoiding_triples_2, avoiding_triples_3, 
    complete_rows, complete_cols, 
    avoid_rows_dup, avoid_cols_dup, 
    get_all_rows_and_cols(B2),
    (are_lists_equal(B1, B2), !; solve_cycler). 

solve :- 
    clear,
    solve_cycler,
    nl,
    print_board,
    nl,
    (\+ solved, write("unable to solve!"); write("solved successfully.")), !.