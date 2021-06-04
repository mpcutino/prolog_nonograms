:- use_module("initialize_board.pl", [
        put_row_color/4,
        put_col_color/4,
        possible_colors/3
    ]).
:- use_module("fill.pl", [
        fill/2,
        solution/3
    ]).


play(RColors, CColors):- 
    length(RColors, RDims), length(CColors, CDims), 
    %forall(nth1(RIndex, RColors, Row), put_row_color(Row, RIndex, 1, CDims)), 
    forall(nth1(CIndex, CColors, Col), put_col_color(Col, 1, CIndex, RDims)),
    fill(RColors, CColors).
