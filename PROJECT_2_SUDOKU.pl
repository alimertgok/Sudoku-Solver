% Step 1: Define the Sudoku grid and constraints.

% The Sudoku grid is represented as a 9x9 list of lists.
% The unassigned cells are represented by variables, and assigned cells are integers from 1 to 9.
% An empty cell is represented by the variable "empty".

:- use_module(library(clpfd)).

% Step 2: Main predicate for solving the Sudoku puzzle.

solve_sudoku(Grid) :-
    % Flatten the grid into a list of variables.
    flatten(Grid, Variables),
    % Variables must be integers between 1 and 9 or unassigned (represented by -1).
    Variables ins 1..9,
    % Apply the Sudoku constraints.
    apply_sudoku_constraints(Grid),
    % Use labeling to find a solution for the Sudoku puzzle.
    labeling([], Variables).

% Step 3: Helper predicates.

% Apply Sudoku constraints to rows, columns, and 3x3 subgrids.
apply_sudoku_constraints(Grid) :-
    % Apply constraints to rows.
    apply_row_constraints(Grid),
    % Transpose the grid to apply constraints to columns.
    transpose(Grid, TransposedGrid),
    apply_row_constraints(TransposedGrid),
    % Apply constraints to 3x3 subgrids.
    apply_subgrid_constraints(Grid).

% Apply constraints to each row.
apply_row_constraints([]).
apply_row_constraints([Row | RestRows]) :-
    all_distinct(Row),
    apply_row_constraints(RestRows).

% Apply constraints to 3x3 subgrids.
apply_subgrid_constraints(Grid) :-
    % Split the rows into three groups (top, middle, bottom).
    divide_into_three(Grid, Top, Middle, Bottom),
    % Split each group of rows into three groups (left, center, right).
    divide_into_three(Top, TopLeft, TopCenter, TopRight),
    divide_into_three(Middle, MiddleLeft, MiddleCenter, MiddleRight),
    divide_into_three(Bottom, BottomLeft, BottomCenter, BottomRight),
    % Check constraints for each subgrid.
    check_subgrid_constraints(TopLeft),
    check_subgrid_constraints(TopCenter),
    check_subgrid_constraints(TopRight),
    check_subgrid_constraints(MiddleLeft),
    check_subgrid_constraints(MiddleCenter),
    check_subgrid_constraints(MiddleRight),
    check_subgrid_constraints(BottomLeft),
    check_subgrid_constraints(BottomCenter),
    check_subgrid_constraints(BottomRight).

% Check constraints for a 3x3 subgrid.
check_subgrid_constraints(Subgrid) :-
    flatten(Subgrid, Variables),
    all_distinct(Variables).

% Divide a list into three parts.
divide_into_three(List, Part1, Part2, Part3) :-
    append(Part1, Part2And3, List),
    length(Part1, Len),
    Len > 0,
    append(Part2, Part3, Part2And3).

% Step 4: Helper predicate to print the Sudoku grid nicely.
% Note: This predicate is optional and only for visualization purposes.

print_sudoku([]).
print_sudoku([Row | RestRows]) :-
    print_row(Row),
    print_sudoku(RestRows).

print_row([]) :-
    write('\n').
print_row([Cell | RestCells]) :-
    (var(Cell) -> write(' _ ') ; write(' '),
     write(Cell),
     write(' ')),
    print_row(RestCells).

% Step 5: Example Sudoku puzzle to solve.

example_sudoku([
    [5, 3, _, _, 7, _, _, _, _],
    [6, _, _, 1, 9, 5, _, _, _],
    [_, 9, 8, _, _, _, _, 6, _],
    [8, _, _, _, 6, _, _, _, 3],
    [4, _, _, 8, _, 3, _, _, 1],
    [7, _, _, _, 2, _, _, _, 6],
    [_, 6, _, _, _, _, 2, 8, _],
    [_, _, _, 4, 1, 9, _, _, 5],
    [_, _, _, _, 8, _, _, 7, 9]
]).

% Step 6: Run the solver on the example Sudoku puzzle.

%:- example_sudoku(Grid), solve_sudoku(Grid), print_sudoku(Grid).
