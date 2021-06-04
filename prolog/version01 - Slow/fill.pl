:- module(fill, 
    [
        fill/2,
        solution/3
    ]
).

:- use_module("initialize_board.pl", [
    possible_colors/3
]).
:- dynamic solution/3.


% this predicate does not fail. If the color selection 
% can not find a minimum, it means that all possible colors 
% has been used, so it is time to finish. The predicate solution/3 
% will have the answers.
%:- findall([X], possible_colors(X, _, _), []), !.
fill(RHeader, CHeader):-
    check_finish(RHeader, CHeader), !.
fill(RHeader, CHeader):-
    % write_ln(RHeader),
    % write_ln(CHeader),

    select_less_colored([X, Y, C]),
    write_ln([X, Y, C]),
    check_valid(X, Y, C, RHeader, CHeader),
    put_color(X, Y, C),
    fill(RHeader, CHeader).

% ==== Color selection ====

% this method only fails if there is not possible color to select
% That's why the postion selected is removed in the last part of the 1st clausule of the predicate. 
% If there is a fail, the selected color is never added again, and the position is only added if there is more colors on it
% This prevent infinite calls
select_less_colored([Cx, Cy, C]):- 
    findall([X, Y, Colors], possible_colors(X, Y, Colors), L),
    lower_colors(L, [Cx, Cy, Cc]),
    member(C, Cc),
    remove_possible_color(Cx, Cy, C, Cc).
% select_less_colored(Cell):- 
%     findall([X, Y, Colors], possible_colors(X, Y, Colors), [_|_]), select_less_colored(Cell). 

lower_colors([[X, Y, C]], [X, Y, C]):-!.
lower_colors([[X, Y, Colors]| R], Cell):- 
    lower_colors(R, [Rx, Ry, Rcolors]), 
    length(Colors, L1), 
    length(Rcolors, L2),
    (
        % I do it like this for efficience, to try to solve more boards
        (L2 < L1, !, Cell = [Rx, Ry, Rcolors]);
        (!, Cell = [X, Y, Colors])
    ).

% === Check Area ====

check_valid(_, _, blank_space, _, _):- !.
check_valid(X, Y, C, RHeader, CHeader):-
    length(RHeader, Rdim),
    length(CHeader, Cdim),
    nth1(X, RHeader, CurrentR),
    write_ln("row check"),
    check_by_row(CurrentR, X, Y, Cdim, C), !,
    nth1(Y, CHeader, CurrentC),
    write_ln("col check"),
    check_by_col(CurrentC, X, Y, Rdim, C).

check_by_row(CurrentRow, Xrow, Ycol, Cdim, Color):- 
    add_to_current_Xdistribution(Xrow, Ycol, Color, Cdim, D),
    subset_check(D, CurrentRow).

check_by_col(CurrentCol, Xrow, Ycol, Rdim, Color):- 
    add_to_current_Ydistribution(Xrow, Ycol, Color, Rdim, D),
    subset_check(D, CurrentCol).

subset_check([], _):- !.
subset_check([[C, I]|R1], [[C, I]|R2]):- !, subset_check(R1, R2).
subset_check([[C, I1]|R1], [[C, I2]|R2]):- I1 < I2, !, Ni is I2 - I1, subset_check(R1, [[C, Ni]|R2]).
subset_check(R1, [[_, _]|R2]):- !, subset_check(R1, R2).


check_finish(RHeader, CHeader):-
    % writeln("in check finish"),
    % row check
    length(CHeader, Cdim),
    numlist(1, Cdim, Cols),
    length(RHeader, Rdim),
    numlist(1, Rdim, Rows),
    findall(
        Ir,
        (
            nth1(Ir, RHeader, Row),
            findall([Ny, C], (member(Ny, Cols), solution(Ir, Ny, C), blank_space \== C), Lr),
            group(Lr, 1, Row)
        ),
        Fullr
    ),
    % writeln("after first find all"),
    length(Fullr, Rdim),
    writeln(Fullr),
    % col check
    findall(
        Ic,
        (
            nth1(Ic, CHeader, Col),
            findall([Nx, C], (member(Nx, Rows), solution(Nx, Ic, C), blank_space \== C), Lc), 
            group(Lc, 1, Col)
        ),
        Fullc
    ),
    % writeln("after second find all"),
    length(Fullc, Cdim),
    writeln(Fullc).


% ==== Auxiliary ====

% the dimension of the column is required to enforce an ordered fetch of solution
add_to_current_Xdistribution(X, Y, Color, Cdim, Distribution):- 
    numlist(1, Cdim, Cols),
    findall([Ny, C], (member(Ny, Cols), solution(X, Ny, C), blank_space \== C), L), 
    group([[Y, Color]|L], 1, Distribution).

add_to_current_Ydistribution(X, Y, Color, Rdim, Distribution):- 
    numlist(1, Rdim, Rows), 
    findall([Nx, C], (member(Nx, Rows), solution(Nx, Y, C), blank_space \== C), L), 
    group([[X, Color]|L], 1, Distribution).

group([], _, R):- !, R=[]. % to force that, if first argument is empty, there will be no more checking in other definitions of the predicate
group(L, Index, R):- maxy(L, Y), Index > Y, !, R=[].
group(L, Index, [[C, Fo]|R]):- member([Index, C], L), Ni is Index + 1, member([Ni, C], L), !, group(L, Ni, [[C, O]|R]), Fo is O+1. 
group(L, Index, [[C, 1]| R]):- member([Index, C], L), !, Ni is Index + 1, group(L, Ni, R).
group(L, Index, R):- Ni is Index + 1, group(L, Ni, R).

maxy([[E, _]], E):- !.
maxy([[E, _]| R], M):- maxy(R, M1), M is max(E, M1).

% === ASSERT-RETRACT predicates ====

put_color(X, Y, C):- assert(solution(X, Y, C)).
put_color(X, Y, C):- %findall([X], possible_colors(X, _, _), [_|_]), 
    retract(solution(X, Y, C)), fail.

remove_possible_color(X, Y, _, R):- 
    retract(possible_colors(X, Y, R)).
remove_possible_color(X, Y, C, R):- 
    assert(possible_colors(X, Y, R)), fail.
    % append(A, [C|B], R), 
    % append(A, B, [L1|L2]), % to check that the resulting list can not be empty
    % assert(possible_colors(X, Y, [L1|L2])), fail.
