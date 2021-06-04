:- use_module("auxiliary.pl", [
        init/2,
        insert/4,
        flatten/2,
        check_by_col/2,
        set_sol/2,
        check_coloration/3,
        sol/2,
        row_info/3
    ]).

play(RColors, CColors):- 
    length(RColors, RDims), length(CColors, CDims), 
    init(RColors, CColors), 
    play_from_dif(CColors, RDims, 0, CDims).

play_from_dif(CColors, Rdim, Top, Top):- 
    !, check_coloration(CColors, 1, Rdim).
play_from_dif(CColors, Rdim, Dif, Top):- 
    row_info(F, L, Dif), not(sol(F, _)), !, 
    insert(L, blank_space, Dif, Candidate), 
    flatten(Candidate, FlatCandidate),
    check_by_col(CColors, FlatCandidate), 
    set_sol(F, FlatCandidate),
    play_from_dif(CColors, Rdim, Dif, Top).
play_from_dif(CColors, Rdim, Dif, Top):- 
    Dif < Top, Nd is Dif + 1, 
    play_from_dif(CColors, Rdim, Nd, Top).

reset :- retractall(sol(_, _)), retractall(row_info(_,_,_)).
