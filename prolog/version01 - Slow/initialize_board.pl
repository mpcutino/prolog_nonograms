:- module(initialize,
          [ 
              put_row_color/4,
              put_col_color/4,
              possible_colors/3
          ]).
:- dynamic possible_colors/3.


sum_index(Color, [[Color, _]|_], 1):- !.
sum_index(_, _, 0):- !.

% this can be improved talking into account continuous colors will have at least space 1.
rest_sum([], 0):- !.
rest_sum([[_, N]|R], Occ):- rest_sum(R, O), Occ is N+O.

try_to_color(X, Y, Color):- possible_colors(X, Y, Colors), member(Color, Colors), !.
try_to_color(X, Y, Color):- possible_colors(X, Y, Colors), !, retract(possible_colors(X, Y, Colors)), assert(possible_colors(X, Y, [Color|Colors])).
try_to_color(X, Y, Color):- assert(possible_colors(X, Y, [Color, blank_space])).

% First add row values, then columns
% indexing on base 1
put_row_color([], _, _, _):- !.
put_row_color([[Color, Number]|R], RIndex, CIndex, CDims):-
    rest_sum(R, Occupancie),
    Top is CDims-Occupancie,
    forall(
        (numlist(CIndex, Top, L), member(Y, L)), 
        try_to_color(RIndex, Y, Color)
    ), sum_index(Color, R, S),
    NewCIndex is CIndex + Number + S, 
    put_row_color(R, RIndex, NewCIndex, CDims).

% Add column values after rows have been added
put_col_color([], _, _, _):- !.
put_col_color([[Color, Number]| R], RIndex, CIndex, RDims):-
    rest_sum(R, Occupancie),
    Top is RDims - Occupancie,
    forall(
        (numlist(RIndex, Top, L), member(X, L)),
        try_to_color(X, CIndex, Color)
    ), sum_index(Color, R, S),
    NewRIndex is RIndex + Number + S,
    put_col_color(R, NewRIndex, CIndex, RDims).
