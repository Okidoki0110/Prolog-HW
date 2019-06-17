
:-ensure_loaded('probleme.pl').
:-ensure_loaded('testing.pl').



% schedule(+Context, -Sol)
% pentru contextul descris, întoarce o soluție care respectă
% constrângerile fizice și de curiculă.
schedule(Context, Sol) :- member(Act, Context), Act = activities([]), Sol = (Context, []).


schedule(_Context, _Sol) :- fail.


% cost(+Sol, -Cost)
% pentru soluția dată, întoarce costul implicat de constrângerile de
% preferință care au fost încălcate.
cost((_, []), 0).
cost(_, _) :- fail.

% schedule_best(+Context, -Sol, -Cost)
% pentru contextul descris, întoarce soluția validă cu cel mai bun (cel
% mai mic) cost (sau una dintre ele, dacă există mai multe cu același
% cost)
schedule_best(Context, (Context, []), 0) :- member(Act, Context), Act = activities([]).
schedule_best(_, _, _) :- fail.













