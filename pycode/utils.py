import json
import numpy as np
from PIL import Image
from pyswip import Atom
import matplotlib.pyplot as plt

from pycode.constants import COLOR_DICT

def clamp(x): 
  return max(0, min(x, 255))


def get_rows_cols(json_path):
    with open(json_path) as fd:
        d = json.load(fd)
    # this is necessary to put a double quote in the color string
    # in this manner pyswip recognize it as a string
    d['rows'] = json.dumps(d['rows'])
    d['cols'] = json.dumps(d['cols'])
    return d['rows'], d['cols']


def show_result(xs, ys, colors, dims):
    h, w = dims
    image = np.full((h, w, 3), 255, dtype=np.uint8)

    for x, y, c in zip(xs, ys, colors):
        color = COLOR_DICT[c.decode()]
        image[x, y, :] = color
    
    img = Image.fromarray(image)
    img = img.resize((w*100, h*100))
    img.show()


def show_results(xs, ys, colors, dims, row_header=None, col_header=None):
    blank_space = Atom("blank_space")
    h, w = dims
    table_colors = [["w" for _ in range(w)] for _ in range(h)]
    for x, y, c in zip(xs, ys, colors):
        if c == blank_space or c == "blank_space": continue
        t = COLOR_DICT[c.decode()]
        if isinstance(t, tuple):
            r, g, b = t
            t = "#{0:02x}{1:02x}{2:02x}".format(clamp(r), clamp(g), clamp(b))
        table_colors[x][y] = t
    cell_text = [["" for _ in range(w)] for _ in range(h)]
    if row_header:
        for i in range(1, h):
            cell_text[i][0] = row_header[i-1]
    if col_header:
        cell_text[0][1:] = col_header

    fig, ax = plt.subplots()
    ax.axis('tight')
    ax.axis('off')
    the_table = ax.table(cellText=cell_text, cellColours=table_colors,loc='center')

    plt.show()


def get_header(r):
    h = []
    for row in r:
        header = []
        for pair in row:
            color, number = pair
            header.append("({0}, {1})".format(color, number))
        h.append("|".join(header))
    return h


def faster(prolog):
    xs, ys, colors = [], [], []
    for r in prolog.query('sol(X, Y)'):
        # the predicates are executed only when they are iterated
        print(r)
        for i, c in enumerate(r['Y']):
            xs.append(r['X'])
            ys.append(i+1)
            colors.append(c)

    return xs, ys, colors


def sloooow(prolog):
    xs, ys, colors = [], [], []
    for r in prolog.query('solution(X, Y, L)'):
        # the predicates are executed only when they are iterated
        print(r)
        xs.append(r['X'])
        ys.append(r['Y'])
        colors.append(r['L'])

    return xs, ys, colors
