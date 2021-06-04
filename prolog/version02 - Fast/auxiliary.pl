:- module(auxiliary,
        [ 
            init/2,
            insert/4,
            flatten/2,
            check_by_col/2,
            check_coloration/3,
            set_sol/2,
            sol/2,
            row_info/3
        ]).
:- dynamic row_info/3, sol/2.

% writing information about the row: row number, row colors with blankspace added (if needed), numbers of cells that can be filled.
init(RColors, CColors):- 
    length(CColors, CDims), 
    forall(
        (
            nth1(F, RColors, Elem), fill(Elem, Nr), get_sum(Elem, Sum), D is CDims - Sum
        ),  
        assert(row_info(F, Nr, D))
    ).

% insert N X times on L. It can obtain all possible forms of insertion using ;
insert(L, _, 0, L):-!.
insert(L, N, X, R):- 
    append(A, B, L),
    X1 is X - 1, insert(B, N, X1, R1), 
    append(A, [N|R1], R).

% sum color in the list using is amount value. In case of consecutive equal color, add also 1.
get_sum([], 0):- !.
get_sum([ [C1, Cant1] | [[C1, Cant2]| R] ], Cantidad):- !, get_sum([[C1, Cant2]| R], Sr), Cantidad is Sr + Cant1 + 1.
get_sum([ [_, Cant] | R], Cantidad):- get_sum(R, C), Cantidad is C + Cant.

% put a blank space between consecutive equal colors
fill([], []):-!.
fill([[C, Cant1]| [[C, Cant2] | R]], F):- 
    !, fill([[C, Cant2] | R], F1), 
    append([[C, Cant1]], [blank_space|F1], F).
fill([[C, Cant]| R], [[C, Cant]|F]):- 
    fill(R, F).%, 
    %append([[C, Cant]], F1, F).

% unifies F with a flat list will colors matching order and quantity of L specification
flatten([], []):-!.
flatten([[C, Cant]|R], F):- !, to_list(C, Cant, L), flatten(R, F1), append(L, F1, F).
flatten([blank_space|R], [blank_space|F]):- flatten(R, F).

to_list(_, Cant, []):- Cant =< 0, !.
to_list(C, Cant, [C|L]):- Cant1 is Cant - 1, to_list(C, Cant1, L).

% ==== Check Area ============

check_coloration(CColors, Index, _):- length(CColors, Cdim), Cdim < Index, !.
check_coloration(CColors, Index, Rdim):- 
    obtain_col_colors(Index, Rdim, IndexColors),
    filter_blank_space(IndexColors, TrueColors),
    nth1(Index, CColors, TrueColors), 
    Ni is Index + 1, 
    check_coloration(CColors, Ni, Rdim).

check_by_col(CColors, F):-
    %write_ln(CColors), write_ln(F),
    %length(CColors, Cl), numlist(1, Cl, LC), member(C, LC), 
    nth1(C, CColors, Elem), nth1(C, F, FColor), not(is_in(FColor, Elem)), !, fail.
check_by_col(_, _).

is_in(blank_space, _) :- !.
is_in(Color, [[Color, _]|_]) :- !.
is_in(Color, [_|Y]) :- is_in(Color, Y).

obtain_col_colors(ColIndex, 1, [[Color, 1]]):- !, sol(1, Rs), nth1(ColIndex, Rs, Color).
obtain_col_colors(ColIndex, RowIndex, R):- sol(RowIndex, Rc), nth1(ColIndex, Rc, Color), Nr is RowIndex - 1, obtain_col_colors(ColIndex, Nr, Nd), merge_colors(Nd, Color, R).

merge_colors(R, Color, Nr):- append(A, [[Color, Cant]], R), !, Nc is Cant + 1, append(A, [[Color, Nc]], Nr).
merge_colors(L, Color, R):- append(L, [[Color, 1]], R).

filter_blank_space([], []):- !.
filter_blank_space([[blank_space, _]|R], TrueColors):- !, filter_blank_space(R, TrueColors).
filter_blank_space([[C, Cant]|R], [[C, Cant]|L]):- filter_blank_space(R, L).

% === Assert and retract =====

set_sol(F, NF) :- assert(sol(F, NF)).
set_sol(F, NF) :- retract(sol(F, NF)), fail.
