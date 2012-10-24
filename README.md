tictactoe.el
============

A simple tictactoe game in emacs lisp

start with an empty board which is an array of arrays

make a move
input a list with one board
output a list with an array of all possible outcomes
       - no nils
       - if there is any solution return a list of only that solution

1 filter out solution games
2 filter out list of non-solution games and find next steps for these
add 1 and 2 together

further optimization:
remove identical nodes. i.e remove all the leaves that have the same end node