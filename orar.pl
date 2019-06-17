
:-ensure_loaded('probleme.pl').
:-ensure_loaded('testing.pl').



% schedule(+Context, -Sol)
% pentru contextul descris, întoarce o soluție care respectă
% constrângerile fizice și de curiculă.
% daca nu este una din cele 4 ar trebui sa puna 
schedule(Context, (Context, [])) :- get_activities(Context, Act), Act = [], !.

schedule(Context, (Context, [])) :- get_rooms(Context, Rooms), Rooms = [], !.
schedule(Context, (Context, [])) :- get_staff(Context, Staff), Staff = [], !.
schedule(Context, (Context, [])) :- get_groups(Context, Groups), Groups = [], !.

% make_list(+Item, +Number, -List) 
% fac o lista continand Item de Number dati

% lista goala
make_list(_, 0, []).

% creaza lista recursiv    
make_list(Item, Number, List) :-
     Prev is Number -1,
     make_list(Item, Prev, L),
     List = [Item | L].

% expand_act(+List_to_expand, -Expanded_list)
% [(sport, 2), (matematica, 3)] va fi expandata in [sport, sport, matematica, matematica, matematica]

% expandarea unei liste goale
expand_act([], []).

expand_act([(A, Index)|Rest], Result) :-
      make_list(A, Index, L),
      expand_act(Rest, R),
      append(L, R, Result).
    
% get_days(+Context, -Days)
% pentru a lua Days din Context
get_days(Context, Days) :-
    member(T, Context),
    T = days(Days).

% get_times(+Context, -Times)
% pentru a lua Times din Context    
get_times(Context, Times) :-
    member(T, Context),
    T = times(Times).
    
% get_groups(+Context, -Groups)
% pentru a lua Groups din Context
get_groups(Context, Groups) :-
    member(T, Context),
    T = groups(Groups).

% get_staff(+Context, -Staff)
% pentru a lua Staff din Context 
get_staff(Context, Staff) :-
    member(T, Context),
    T = staff(Staff).

% get_activities(+Context, -Activities)
% pentru a lua Activities din Context  
get_activities(Context, Activities) :-
    member(T, Context),
    T = activities(Activities).
    
% get_constraints(+Context, -Constraints)
% pentru a lua Constraints din Context 
get_constraints(Context, Constraints) :-
    member(T, Context),
    T = constraints(Constraints).
    
% get_rooms(+Context, -Rooms)
% pentru a lua Rooms din Context 
get_rooms(Context, Rooms) :-
    member(T, Context),
    T = rooms(Rooms).
    
% foloseste solve ca sa gaseasca orarele
schedule(Context, (Context, Slots)) :-
    solve(Context, Slots).
    
schedule(_, _) :- fail.

% solve(+Context, -Sol)
% gaseste toate orarele pentru context
solve(Context, Sol) :-
    get_activities(Context, Activities), % ia activitatile (exemplu: [(matematica, 2), (sport, 1)])
    expand_act(Activities, Acts), % expandeaza activitatile (exemplu: [matematica, matematica, sport])
    get_groups(Context, Groups), % ia grupele (exemplu: [va, vb])
    findall((A,G), (member(A,Acts), member(G,Groups)), AG), % construieste toate perechile (activitate, grupa)
    !,  % (exemplu: [(matematica, va), (matematica, va), (sport, va), (matematica, vb), (matematica, vb), (sport, vb)]  )
    solve2(Context, AG, [], Sol). % cheama solve2

% solve2(+Context, +AGList, +Init, -Result) 
% construieste toate orarele pentru un anumit Context, avand si o solutie partiala de plecare
% +Context = contextul dat
% +AGList = toate perechile (activitate, grupa) obtinute din Context
% +Init = acumulator cu solutia partiala gasita
% -Result = orarul rezultat

% cazul cand AGList e vid
solve2(_, [], _, []) :- !.

% solve2 se apeleaza recursiv, si adauga un slot pentru perechea AG curenta
solve2(Context, [AG|Rest], Init, Result) :-
     solve2(Context, Rest, Init, Result1),
     add_slot(Context, AG, Result1, Result).

% add_slot(+Context, +AG, +Node, -Next_Node)
% creaza o noua lista Next_Node, care contine un nou slot, suplimentar fata de cele continute in lista Node
% Node este lista de slot-uri adaugate pana acum
% Next_Node este lista Node in care s-a mai adaugat inca un slot
% AG este o pereche (A, G) de activitate si grupa 
add_slot(Context, (A,G), Node, Next_Node) :-
    get_days(Context, Days), 
    get_times(Context, Times),
    get_rooms(Context, Rooms), 
    get_staff(Context, Staff),
    member(D, Days),
    member(T, Times),
    member(R, Rooms),
    member((P, Spec), Staff),
    member(A, Spec),  % verifica ca activitatea face parte dintre activitatile profesorului
    Slot = slot(A, G, D, T, R, P),
    \+ member(slot(_,G,D,T,_,_), Node), % verifica ca grupa nu este deja ocupata in acel moment in lista Node
    \+ member(slot(_,_,D,T,_,P), Node), % verifica ca profesorul nu este deja ocupat in acel moment in lista Node
    \+ member(slot(_,_,D,T,R,_), Node), % verifica ca sala nu este deja ocupata in acel moment in lista Node
    Next_Node = [Slot| Node]. % construieste Next_Node ca fiind Node plus Slot
    


% cost(+Sol, -Cost)
% pentru soluția dată, întoarce costul implicat de constrângerile de
% preferință care au fost încălcate.
cost((_, []), 0). 
/*
cost((_, [_]), 0).
*/

cost(_, _) :- fail.

% schedule_best(+Context, -Sol, -Cost)
% pentru contextul descris, întoarce soluția validă cu cel mai bun (cel
% mai mic) cost (sau una dintre ele, dacă există mai multe cu același
% cost)
schedule_best(Context, (Context, []), 0) :- get_activities(Context, Activities), Activities = [].
/*
schedule_best(Context, Schedule, Cost) :- schedule(Context, S), cost(S, C), Cost = C.
*/

schedule_best(_, _, _) :- fail.













