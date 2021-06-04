import time
from pyswip import Prolog
from ast import literal_eval

from pycode.utils import *


def main(knowledge_base, json_board, fast=True):
    rows, cols = get_rows_cols(json_board)

    # compute prolog solution to the current board
    prolog = Prolog()
    prolog.consult(knowledge_base)
    now = time.time()
    for r in prolog.query('play({0}, {1})'.format(rows, cols)):
        # the predicates are executed only when they are iterated
        break
    elapsed_t = time.time() - now
    print(elapsed_t)
    
    xs, ys, colors = faster(prolog) if fast else sloooow(prolog)
    
    # prepare to show results
    rows = literal_eval(rows)
    cols = literal_eval(cols)
    r_h = get_header(rows)
    c_h = get_header(cols)
    show_results(xs, ys, colors, (len(rows) + 1, 1 + len(cols)), row_header=r_h, col_header=c_h)


if __name__ == "__main__":
    # main("prolog/version02 - Fast/main.pl", "boards/sohei1/12.json")
    main("prolog/version02 - Fast/main.pl", "boards/sohei2/24.json")
