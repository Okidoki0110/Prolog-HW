
:-ensure_loaded('probleme.pl').
:-ensure_loaded('testing.pl').



% schedule(+Context, -Sol)
% pentru contextul descris, întoarce o soluție care respectă
% constrângerile fizice și de curiculă.
schedule(_, _) :- fail.

% schedule(+Context, -Sol, -Cost)
% pentru contextul descris, întoarce o soluție care respectă
% constrângerile fizice și de curiculă și calculează costul implicat de
% constrângerile de preferință care au fost încălcate.
schedule(_, _, _) :- fail.

% schedule_best(+Context, -Sol, -Cost)
% pentru contextul descris, întoarce soluția validă cu cel mai bun (cel
% mai mic) cost (sau una dintre ele, dacă există mai multe cu același
% cost)
schedule_best(_, _, _) :- fail.













