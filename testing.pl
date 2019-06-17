
:-ensure_loaded('probleme.pl').
:-ensure_loaded('orar.pl').


vmpoints(Test, Points) :-
    member(Test:Points,
           [ zero:5
           , unu:10
           , no_resources:10
           , nsols:15
           , maxI:10
           , maxH:5
           , minH:5
           , cont:10
           , interval:10
           , best:20
           , bonus:20
           ]).

% nimic de programat

tt(zero, [
       exp('(problem(zero, PD), schedule(PD, PDSched))', [
               cond('schedCheck(PD, PDSched)'),
               cond('PDSched = (_, [])')
           ]),
       nsl('(problem(zero, PD), schedule(PD, PDSched))', 'PDSched', 1)
   ]).


% probleme simple

tt(unu, TT) :- ttnsols([(unu1, 8), (unu2, 48), (unu3, 8)], TT).

% resurse insuficiente

tt(no_resources, TT) :-
    findall((P, 0), member(P, [no_staff, no_time, no_room]), Pbs),
    ttnsols([(zero, 1) | Pbs], TT).

% verificare că soluțiile date nu sunt duplicate

tt(nsols, TT) :- ttnsols([(sameSmall, 1), (sameBigNone, 120), (sameBigSome, 30)], TT).

% verificare max_instances

tt(maxI, TT) :- ttnsols([(maxi, 432), (maxiNo, 0), (maxi2, 81)], TT).

% verificare max_hours

tt(maxH, [
       exp('problem(minmax1, PD), cost(([constraints([max_hours(marcel, 2, 5), max_hours(kobayashi, 3, 10)]) | PD], [slot(sport,grupa1,lu,(8-9,1),sala,marcel),slot(sport,grupa1,lu,(9-10,1),sala,marcel),slot(karate,grupa1,lu,(10-11,1),sala,marcel),slot(karate,grupa1,lu,(11-12,1),sala,marcel)]), C)', ['C', 10]),
       exp('problem(minmax1, PD), cost(([constraints([max_hours(marcel, 2, 5), max_hours(kobayashi, 2, 7)]) | PD], [slot(sport,grupa1,lu,(10-11,1),sala,kobayashi),slot(sport,grupa1,lu,(11-12,1),sala,kobayashi),slot(karate,grupa1,lu,(8-9,1),sala,kobayashi),slot(karate,grupa1,lu,(9-10,1),sala,marcel)]), C)', ['C', 7]),
       exp('problem(minmax2, PD), cost(([constraints([max_hours(andrei, 2, 5)]) | PD], [slot(pp,grupa1,lu,(8-10,2),eg105,andrei),slot(pp,grupa2,lu,(8-10,2),eg104,mihnea),slot(pp,grupa1,lu,(10-12,2),eg105,andrei),slot(pp,grupa2,lu,(10-12,2),eg104,mihnea),slot(pp,grupa1,lu,(12-14,2),eg105,andrei),slot(pp,grupa2,lu,(12-14,2),eg104,mihnea),slot(ia,grupa1,lu,(14-16,2),eg105,andrei),slot(ia,grupa2,lu,(14-16,2),eg104,tudor),slot(ia,grupa1,lu,(16-18,2),eg105,andrei),slot(ia,grupa2,lu,(16-18,2),eg104,tudor)]), C)', ['C', 40])
   ]).

% verificare min_hours

tt(minH, [
       exp('problem(minmax1, PD), cost(([constraints([min_hours(marcel, 2, 5), min_hours(kobayashi, 3, 10)]) | PD], [slot(sport,grupa1,lu,(8-9,1),sala,marcel),slot(sport,grupa1,lu,(9-10,1),sala,marcel),slot(karate,grupa1,lu,(10-11,1),sala,marcel),slot(karate,grupa1,lu,(11-12,1),sala,marcel)]), C)', ['C', 30]),
       exp('problem(minmax1, PD), cost(([constraints([min_hours(marcel, 2, 5), min_hours(kobayashi, 2, 7)]) | PD], [slot(sport,grupa1,lu,(10-11,1),sala,kobayashi),slot(sport,grupa1,lu,(11-12,1),sala,kobayashi),slot(karate,grupa1,lu,(8-9,1),sala,kobayashi),slot(karate,grupa1,lu,(9-10,1),sala,marcel)]), C)', ['C', 5]),
       exp('problem(minmax2, PD), cost(([constraints([min_hours(eg104, 12, 1), min_hours(grupa2, 12, 1), min_hours(tudor, 6, 3)]) | PD], [slot(pp,grupa1,lu,(8-10,2),eg105,andrei),slot(pp,grupa2,lu,(8-10,2),eg104,mihnea),slot(pp,grupa1,lu,(10-12,2),eg105,andrei),slot(pp,grupa2,lu,(10-12,2),eg104,mihnea),slot(pp,grupa1,lu,(12-14,2),eg105,andrei),slot(pp,grupa2,lu,(12-14,2),eg104,mihnea),slot(ia,grupa1,lu,(14-16,2),eg105,andrei),slot(ia,grupa2,lu,(14-16,2),eg104,tudor),slot(ia,grupa1,lu,(16-18,2),eg105,andrei),slot(ia,grupa2,lu,(16-18,2),eg104,tudor)]), C)', ['C', 52])
   ]).

% verificare continuous

tt(cont, [
       exp('problem(continuums, PD), cost(([constraints([continuous(grupa1, 1)]) | PD], [slot(pp,grupa1,lu,(8-10,2),eg105,andrei),slot(pp,grupa2,lu,(10-12,2),eg105,andrei),slot(pp,grupa1,lu,(12-14,2),eg105,andrei),slot(pp,grupa2,lu,(14-16,2),eg105,andrei),slot(pp,grupa1,lu,(16-18,2),eg105,andrei),slot(pp,grupa2,lu,(18-20,2),eg105,andrei),slot(pp,grupa1,ma,(8-10,2),eg105,andrei),slot(pp,grupa2,ma,(10-12,2),eg105,andrei),slot(pp,grupa1,ma,(12-14,2),eg105,andrei),slot(pp,grupa2,ma,(14-16,2),eg105,andrei),slot(pp,grupa1,ma,(16-18,2),eg105,andrei),slot(pp,grupa2,ma,(18-20,2),eg105,andrei),slot(pp,grupa1,mi,(8-10,2),eg105,andrei),slot(pp,grupa2,mi,(10-12,2),eg105,andrei),slot(pp,grupa1,mi,(12-14,2),eg105,andrei),slot(pp,grupa2,mi,(14-16,2),eg105,andrei)]), C)', ['C', 5]),
       exp('problem(continuums, PD), cost(([constraints([continuous(grupa1, 1), continuous(grupa2, 2), continuous(andrei, 2), continuous(eg105, 10)]) | PD], [slot(pp,grupa1,lu,(8-10,2),eg105,andrei),slot(pp,grupa2,lu,(10-12,2),eg105,andrei),slot(pp,grupa1,lu,(12-14,2),eg105,andrei),slot(pp,grupa2,lu,(14-16,2),eg105,andrei),slot(pp,grupa1,lu,(16-18,2),eg105,andrei),slot(pp,grupa2,lu,(18-20,2),eg105,andrei),slot(pp,grupa1,ma,(8-10,2),eg105,andrei),slot(pp,grupa2,ma,(10-12,2),eg105,andrei),slot(pp,grupa1,ma,(12-14,2),eg105,andrei),slot(pp,grupa2,ma,(14-16,2),eg105,andrei),slot(pp,grupa1,ma,(16-18,2),eg105,andrei),slot(pp,grupa2,ma,(18-20,2),eg105,andrei),slot(pp,grupa1,mi,(8-10,2),eg105,andrei),slot(pp,grupa2,mi,(10-12,2),eg105,andrei),slot(pp,grupa1,mi,(12-14,2),eg105,andrei),slot(pp,grupa2,mi,(14-16,2),eg105,andrei)]), C)', ['C', 15]),
       exp('problem(continuums2, PD), cost(([constraints([continuous(grupa1, 1), continuous(grupa2, 1), continuous(andrei, 5), continuous(mihnea, 5), continuous(eg105, 10)]) | PD], [slot(pp,grupa1,lu,(8-10,2),eg105,andrei),slot(pp,grupa2,lu,(10-12,2),eg105,andrei),slot(pp,grupa1,lu,(12-14,2),eg105,andrei),slot(pp,grupa2,lu,(14-16,2),eg105,andrei),slot(pp,grupa1,ma,(8-10,2),eg105,andrei),slot(pp,grupa2,ma,(12-14,2),eg105,andrei),slot(cpl,grupa1,lu,(16-18,2),eg105,mihnea),slot(cpl,grupa2,lu,(18-20,2),eg105,mihnea),slot(cpl,grupa1,ma,(10-12,2),eg105,mihnea),slot(cpl,grupa2,ma,(14-16,2),eg105,mihnea),slot(cpl,grupa1,ma,(16-18,2),eg105,mihnea),slot(cpl,grupa2,ma,(18-20,2),eg105,mihnea)]), C)', ['C', 16])
   ]).

% verificare interval

tt(interval, [
       exp('problem(continuums2, PD), cost(([constraints([interval(andrei, 8, 14, 1), interval(mihnea, 12, 18, 5), interval(grupa1, 8, 14, 10)]) | PD], [slot(pp,grupa1,lu,(8-10,2),eg105,andrei),slot(pp,grupa2,lu,(10-12,2),eg105,andrei),slot(pp,grupa1,lu,(12-14,2),eg105,andrei),slot(pp,grupa2,lu,(14-16,2),eg105,andrei),slot(pp,grupa1,ma,(8-10,2),eg105,andrei),slot(pp,grupa2,lu,(18-20,2),eg105,andrei),slot(cpl,grupa1,ma,(10-12,2),eg105,mihnea),slot(cpl,grupa2,lu,(16-18,2),eg105,mihnea),slot(cpl,grupa1,ma,(14-16,2),eg105,mihnea),slot(cpl,grupa2,ma,(12-14,2),eg105,mihnea),slot(cpl,grupa1,ma,(18-20,2),eg105,mihnea),slot(cpl,grupa2,ma,(16-18,2),eg105,mihnea)]), C)', ['C', 108])
   ]).
% 1 * (2 + 6) + 5 * (2 + 2) + 10 * (2 + 6) = 108

% verificare că soluția cea mai bună este întoarsă corect, pentru
% probleme simple. Soluția este unică pentru acest probleme.

tt(best, [
       wait,
       exp('problem(maxiBestZero, PD), schedule_best(PD, (_, Slots), Cost)',
           [set('Slots', [slot(pp,grupa1,lu,(10-12,2),eg105,mihaela),slot(pp,grupa1,lu,(12-14,2),eg105,mihaela),slot(pp,grupa1,ma,(10-12,2),eg105,mihaela),slot(pp,grupa1,ma,(12-14,2),eg105,mihaela),slot(pp,grupa1,mi,(10-12,2),eg105,mihaela),slot(pp,grupa1,mi,(12-14,2),eg105,mihaela),slot(cpl,grupa1,lu,(8-10,2),eg105,mihnea),slot(cpl,grupa1,ma,(8-10,2),eg105,mihnea),slot(cpl,grupa1,mi,(8-10,2),eg105,mihnea)]),
            'Cost', 0]),
       wait,
       exp('problem(continuumsBest, PD), schedule_best(PD, (_, Slots), Cost)',
           [set('Slots', [slot(pp,grupa1,lu,(12-14,2),eg105,andrei),slot(pp,grupa2,lu,(10-12,2),eg105,andrei),slot(pp,grupa1,ma,(12-14,2),eg105,andrei),slot(pp,grupa2,ma,(10-12,2),eg105,andrei),slot(cpl,grupa1,lu,(10-12,2),eg104,mihnea),slot(cpl,grupa2,lu,(8-10,2),eg104,mihnea),slot(cpl,grupa1,ma,(10-12,2),eg104,mihnea),slot(cpl,grupa2,ma,(8-10,2),eg104,mihnea)]),
            'Cost', 12])
   ]).

%   BONUS

tt(bonus, [
       wait,
    2, exp('problem(hard, PD), schedule_best(PD, Sol, Cost)',
           [cond('schedCheck(PD, Sol)'),
           'Cost', 0
          ]),
       wait,
       exp('problem(allZero, PD), schedule_best(PD, Sol, Cost)',
           [cond('schedCheck(PD, Sol)'),
           'Cost', 0
          ]),
       wait,
       exp('problem(all, PD), schedule_best(PD, Sol, Cost)',
           [cond('schedCheck(PD, Sol)'),
            'Cost', 9
           ])
   ]).









% Utilizare internă


ttnsols(Pbs, TT) :-
    findall([For, Nsl], (
                member((Pb, NSols), Pbs),
                reverse(['(problem(', Pb, ', PD), schedule(PD, PDSched))'], RQS),
                foldl(string_concat, RQS, '', Q),
                For = ech(Q, ['schedCheck(PD, PDSched)']),
                Nsl = nsl(Q, 'PDSched', NSols)),
            Testss),
    flatten(Testss, TT).


schedCheck(PDGiven, PDSched) :-
    problem(Pb, PDGiven), problemcheck(Pb),
    (   PDSched = (PD, Sched), !,
        (   sort(PD, Same),
            sort(PDGiven, Same), !,
            member(activities(AA2), PD),
            findall(A, member((A, _), AA2), AA),
            member(groups(GG), PD),
            member(days(DD), PD),
            member(times(TT), PD),
            member(rooms(RR), PD),
            member(staff(PP2), PD),
            findall(P, member((P, _), PP2), PP),
            (   is_list(Sched), !,
                forall(member(Slot, Sched),
                       (   Slot = slot(A, G, D, T, R, P), !,
                           checkSlot(A, AA, Slot, 'o activitate'),
                           checkSlot(G, GG, Slot, 'o grupa'),
                           checkSlot(D, DD, Slot, 'o zi'),
                           checkSlot(T, TT, Slot, 'un interval'),
                           checkSlot(R, RR, Slot, 'o sala'),
                           checkSlot(P, PP, Slot, 'o persoana'),
                           checkP(Slot, Sched), checkR(Slot, Sched), checkG(Slot, Sched)
                       ;   format('O intrare din orar nu este in format corect: ~w~n',
                                    [Slot]), fail
                       )),
                forall((member(G, GG), member((A, NMust), AA2)),
                       (   findall(A:G, member(slot(A, G, _, _, _, _), Sched), L),
                           length(L, NDone),
                           (   NDone =:= NMust
                           ;   format('Pentru activitatea ~w trebuiau programate ~w instante dar au fost programate ~w.~n', [NMust, NDone])
                           )))
            ;   format('Orarul nu este o lista: ~w~n', [Sched]), fail
            )
        ;   format('Rezultatul nu contine descrierea problemei: ~w~n', [PDSched]), fail
        )
    ;   format('Rezultatul nu este o pereche: ~w~n', [PDSched]), fail
    ).

checkSlot(X, LX, _, _) :- member(X, LX), !.
checkSlot(X, _, Slot, Name) :-
    format('Elementul ~w din ~w nu este ~w~n', [X, Slot, Name]),
    fail.
checkP(slot(A, G, D, T, R, P), Sched) :- findall(D:T, member(slot(_,_,D,T,_,P), Sched), L),
    (   L = [_,_|_],
        format('Slotul ~w se suprapune cu alte sloturi pentru persoana ~w: ~w~n',
              [slot(A, G, D, T, R, P), P, L]), fail
    ;   true).
checkG(slot(A, G, D, T, R, P), Sched) :- findall(D:T, member(slot(_,G,D,T,_,_), Sched), L),
    (   L = [_,_|_],
        format('Slotul ~w se suprapune cu alte sloturi pentru grupa ~w: ~w~n',
              [slot(A, G, D, T, R, P), G, L]), fail
    ;   true).
checkR(slot(A, G, D, T, R, P), Sched) :- findall(D:T, member(slot(_,_,D,T,R,_), Sched), L),
    (   L = [_,_|_],
        format('Slotul ~w se suprapune cu alte sloturi pentru perosana ~w: ~w~n',
              [slot(A, G, D, T, R, P), R, L]), fail
    ;   true).

%% ----------------------------------------
%% ----------------------------------------

problemcheck(Pb) :- problem(Pb, PD),
    EE = [days(X), times(X), rooms(X), groups(X), activities(X), staff(X)],
    forall(member(E, EE),
           (   member(E, PD),
               (   is_list(X)
               ;   format('INTERN: Not a list ~w in ~w.~n', [X, Pb]))
           ;   format('INTERN: Member ~w missing in ~w.~n', [E, Pb]))),
    forall(member(E, PD),
           (   member(E, [constraints(X) | EE])
           ;   format('INTERN: Member unknown: ~w in ~w.~n', [E, Pb]))),
    member(times(TT), PD),
    forall(member(T, TT),
           (   T = (_-_, _)
           ;   format('INTERN: Incorrect time format ~w in ~w.~n', [T, Pb]))),
    member(activities(AA), PD),
    forall(member(A, AA),
           (   A = (_, _)
           ;   format('INTERN: Incorrect activity format ~w in ~w.~n', [A, Pb]))),
    member(staff(SS), PD),
    forall(member(S, SS),
           (   S = (_, As),
               (   is_list(As)
               ;   format('INTERN: Staff property ~w not a list in ~w.~n', [S, Pb]))
           ;   format('INTERN: Incorrect staff format ~w in ~w.~n', [S, Pb]))),
    (   member(constraints(CC), PD),
        (   is_list(CC),
            (   forall(member(C, CC),
                       (   C = max_instances(_, _)
                       ;   C = max_hours(_, _, _)
                       ;   C = min_hours(_, _, _)
                       ;   C = continuous(_, _)
                       ;   C = interval(_, _, _, _)
                       ;   format('INTERN: Incorrect constraint ~w in ~w.~n', [C, Pb])
                )))
        ;   format('INTERN: Constraints not a list in ~w.~n', [Pb]))
    ;   true).



%% ----------------------------------------
%% ----------------------------------------
%% Tester

testtimelimit(30). % în secunde

test_mode(vmchecker).
test_mode(quickcheck) :- \+ test_mode(vmchecker).

:-dynamic(punct/2).
:-dynamic(current/1).
%:-clean.

clean :- retractall(punct(_, _)), retractall(current(_)).


% -----------------

% runs vm tests
vmtest :- checkVm.
vmcheck :- checkVm.
checkVm :-
        clean,
        findall(T:Score, (tt(T, _), vmtest(T, Score)), Results),
        findall(Score, member(_:Score, Results), Scores),
        sum_list(Scores, S),
        format('Total: ~w~n', [S]),
        clean.

% entry point (for users) for individual vm tests.
vmtest(T) :-
        vmtest(T, Score),
        format('Total: ~w.', [Score]).

% performes a vm test, outputs score.
vmtest(T, Score) :-
        once(vmpoints(T, Pts)),
        tt(T, TestList),
        tests(TestList, Pts, T, Score).


% ---------------
% general testing

% unified entry point for testing; computes fractions, computes if
% exercise is not done, and starts per-test iteration.
tests(Tests, TotalPoints, Ex, Score) :- %trace,
    total_units(Tests, TF, Ck/AllCheck, UCk/AllUCk, Others/AllOthers),
    (   isNotDone(Ck/AllCheck, UCk/AllUCk, Others/AllOthers), !,
        (   Ex == none, !
        ;   ( test_mode(vmchecker), !, format("+0.00 ~10t ") ; true ),
            format("[~w] Nerezolvat.~n", [Ex])
        ),
        Score = 0
    ;   Unit is TotalPoints / TF,
        tests(Tests, Ex, 1, Unit, 0, Score)
    ), !.
tests(_, _, Ex, _) :- failure(Ex, 'INTERN: tests/4 failed').

isNotDone(0/TC, TU/TU, 0/TO) :- (TO > 0, !; TC > 0).
% otherwise, probably done

% iterates through tests, handles test index, generates test id, adds
% points
tests([], _, _, _, Points, Points) :- !.
tests([wait|R], Ex, Idx, Unit, PointsIn, PointsOut) :- !,
    tests(R, Ex, Idx, Unit, PointsIn, PointsOut).
tests([Fraction, T|R], Ex, Idx, Unit, PointsIn, PointsOut) :-
        number(Fraction), !, test(T, Ex, Idx, Fraction, Unit, PointsIn, PointsOut1),
        tests(R, Ex, Idx+1, Unit, PointsOut1, PointsOut).
tests([T|R], Ex, Idx, Unit, PointsIn, PointsOut) :-
        test(T, Ex, Idx, 1, Unit, PointsIn, PointsOut1),
        tests(R, Ex, Idx+1, Unit, PointsOut1, PointsOut).
tests(_, Ex, _, _, _, _) :- failure(Ex, 'INTERN: tests/6 failed').

total_units([], 0, 0/0, 0/0, 0/0).
total_units([wait, P, _|R], Tot, A, B, C) :-
    number(P), !, total_units(R, TotR, A, B, C), Tot is TotR + P.
total_units([wait, _|R], Tot, CO/TCO, UO/TUO, OO/TOO) :- !,
    total_units(R, TotR, CO/TCO, UO/TUO, OO/TOO), Tot is TotR + 1.
total_units([P, T|R], Tot, A, B, C) :-
    number(P), !, total_units([T|R], TotR, A, B, C), Tot is TotR + P.
total_units([T|R], Tot, CO/TCO, UO/TUO, OO/TOO) :- %trace,
    test(T, dry, dry, _, _, 0, P),
    (   ( T = chk(_), ! ; T = ckA(_, _) ), !, TA = 1,
        (   P > 0, A = 1, !; A = 0 )
    ;   TA = 0, A = 0),
    (   ( T = uck(_), ! ; T = nsl(_, _, 0) ), !, TB = 1,
        (   P > 0, B = 1, !; B = 0 )
    ;   TB = 0, B = 0),
    (   T \= chk(_), T \= ckA(_, _), T \= uck(_), T \= ech(_, _), T \= nsl(_, _, 0), !,
        TD = 1, (   P > 0, D = 1, !; D = 0 )
    ;   TD = 0, D = 0),
    total_units(R, TotR, C/TC, U/TU, O/TO), Tot is TotR + 1,
    CO is C+A, TCO is TC+TA, UO is U+B, TUO is TU+TB, OO is O+D, TOO is TO+TD.

test(T, NEx, Idx, Fraction, Unit, PointsIn, PointsOut) :-
        (   NEx == dry, !, Ex = dry, TimeLimit = 0.1
        ;   testtimelimit(TimeLimit),
            IdxI is Idx + 96, char_code(CEx, IdxI),
            (   NEx == none, !, swritef(Ex, '%w|', [CEx])
            ;   swritef(Ex, '[%w|%w]', [NEx, CEx]))
        ),
        swritef(MTime, 'limita de %w secunde depasita', [TimeLimit]),
        (   catch(
                catch(call_with_time_limit(TimeLimit, once(test(Ex, T))),
                      time_limit_exceeded,
                      except(Ex, MTime)
                     ),
                Expt,
                (   swritef(M, 'exceptie: %w', [Expt]), except(Ex, M))
            ),
            !, success(Ex, Fraction, Unit, Points),
            PointsOut is PointsIn + Points
        ; PointsOut = PointsIn).
test(_, Ex, Idx, _, _, _, _) :- failure(Ex/Idx, 'INTERN: test/7 failed').

success(dry, _, _, 1) :- !.
success(Ex, Fraction, Unit, Score) :-
    Score is Fraction * Unit,
    (   test_mode(vmchecker), !,
        format('+~2f ~10t ~w Corect.~n', [Score, Ex])
    ;   format('~w[OK] Corect. +~2f.~n', [Ex, Score])).
failure(dry, _) :- !, fail.
failure(Ex, M) :-
        (   test_mode(vmchecker), !,
            format('+0.00 ~10t  ~w ~w~n', [Ex, M]), fail
        ;   format('~w[--] ~w~n', [Ex, M]), fail).
except(dry, _) :- !, fail.
except(Ex, M) :-
        (   test_mode(vmchecker), !,
            format('+0.00 ~10t ~w Exception: ~w~n', [Ex, M]), fail
        ;   format('~w[/-] ~w~n', [Ex, M]), fail).

test(Ex, chk(P)) :- !, testCall(Ex, P).
test(Ex, uck(P)) :- !, testCall(Ex, \+ P).
test(Ex, exp(Text, ExpList)) :- !,
    read_term_from_atom(Text, P, [variable_names(Vars)]),
    testCall(Ex, P, Text), testExp(Ex, Text, Vars, ExpList).
test(_, ckA(_, [])) :- !.
test(Ex, ckA(Pred, [Test|Tests])) :- !,
    swritef(S, '%w(%w)', [Pred, Test]),
    read_term_from_atom(S, P, []),
    testCall(Ex, P, S), test(Ex, ckA(Pred, Tests)).
test(_, ech(_, [])) :- !.
test(Ex, ech(Text, [Cond|Conds])) :- !,
    swritef(S, '%w|%w', [Text, Cond]),
    read_term_from_atom(S, P|Q, [variable_names(Vars)]),
    forall(P, (
               swritef(Msg, '%w pentru soluția %w a predicatului %w', [Cond, Vars, Text]),
               testCall(Ex, Q, Msg))),
    test(Ex, ech(Text, Conds)).
test(Ex, nsl(Text, Tmplt, N)) :- !,
    swritef(S, 'findall(%w, %w, TheList)', [Tmplt, Text]),
    read_term_from_atom(S, P, [variable_names(Vars)]),
    testCall(Ex, P, S), testNSols(Ex, Text, Vars, N).
test(Ex, sls(Text, Tmplt, Sols)) :- !,
    swritef(S, 'findall(%w, %w, TheList)', [Tmplt, Text]),
    read_term_from_atom(S, P, [variable_names(Vars)]),
    testCall(Ex, P, S), testSols(Ex, Text, Vars, Sols).
test(Ex, sSO(Text, Tmplt, Sols)) :- !,
    swritef(S, 'setof(%w, %w, TheList)', [Tmplt, Text]),
    read_term_from_atom(S, P, [variable_names(Vars)]),
    testCall(Ex, P, S), testSols(Ex, Text, Vars, Sols).
test(Ex, _) :- failure(Ex, 'INTERN: Test necunoscut').

% Pentru exercițiul Ex, evaluează clauza Do, dată ca termen.
% Opțional, în mesajul de eroare interogarea poate fi afișată ca
% parametrul Text.
testCall(Ex, Do) :- swritef(Text, '%q', [Do]), testCall(Ex, Do, Text).
testCall(Ex, Do, Text) :-
        catch((call(Do), !
              ;   !, swritef(M, 'Interogarea %w a esuat.', [Text]), failure(Ex, M)
              ), Exc,
              (swritef(M, 'Interogarea %w a produs exceptie: %w', [Text, Exc]),
              except(Ex, M))
             ).

testExp(_, _, _, []) :- !.
testExp(Ex, Text, Vars, [v(Var) | Rest]) :- !,
    (   getVal(Var, Vars, V), !,
        (   var(V), !, testExp(Ex, Text, Vars, Rest) ;
            swritef(M, 'Interogarea %w leaga %w (la valoarea %w) dar nu ar fi trebuit legata.',
                    [Text, Var, V]), failure(Ex, M)
        )
    ;
    swritef(M, 'INTERN: Interogarea %w nu contine variabila %w.', [Text, Var]),
    failure(Ex, M)
    ).
testExp(Ex, Text, Vars, [set(Var, Set) | Rest]) :- !,
    (   getVal(Var, Vars, V), !,
        testSet(Ex, Text, 'intoarce', V, Set),
        testExp(Ex, Text, Vars, Rest)
    ;
    swritef(M, 'INTERN: Interogarea %w nu contine variabila %w.', [Text, Var]),
    failure(Ex, M)
    ).
testExp(Ex, Text, Vars, [setU(Var, Set) | Rest]) :- !,
    (   getVal(Var, Vars, V), !,
        testSetU(Ex, Text, 'intoarce', V, Set),
        testExp(Ex, Text, Vars, Rest)
    ;
    swritef(M, 'INTERN: Interogarea %w nu contine variabila %w.', [Text, Var]),
    failure(Ex, M)
    ).
testExp(Ex, Text, Vars, [cond(Cond) | Rest]) :- !,
    swritef(S, "(%w, %w)", [Text, Cond]),
    read_term_from_atom(S, P, []),
    (
        call(P), !, testExp(Ex, Text, Vars, Rest)
        ;
        swritef(M, 'Dupa interogarea %w conditia %w nu este adevarata.', [Text, Cond]),
        failure(Ex, M)
    ).
testExp(Ex, Text, Vars, [Var, Val | Rest]) :- !,
    (   getVal(Var, Vars, V), !,
        (   V == Val, !, testExp(Ex, Text, Vars, Rest) ;
            swritef(M, 'Interogarea %w leaga %w la %w in loc de %w.',
                    [Text, Var, V, Val]), failure(Ex, M)
        )
    ;
    swritef(M, 'INTERN: Interogarea %w nu contine variabila %w.', [Text, Var]),
    failure(Ex, M)
    ).
testExp(Ex, _, _, [X | _]) :- !,
        swritef(M, 'INTERN: element necunoscut pentru exp: %w', [X]),
        failure(Ex, M).
testExp(Ex, _, _, X) :- !,
        swritef(M, 'INTERN: format gresit pentru exp: %w', [X]),
        failure(Ex, M).

testNSols(Ex, Text, Vars, N) :-
    (   getVal('TheList', Vars, V), length(V, NSols), !,
        (   NSols =:= N, !
        ;   swritef(M, 'Numarul de solutii pentru %w este %w in loc de %w.',
                    [Text, NSols, N]), failure(Ex, M)
        )
    ;   failure(Ex, 'INTERNAL: nu avem variabila TheList sau aceasta nu este lista.')
    ).

testSols(Ex, Text, Vars, Sols) :-
    (   getVal('TheList', Vars, V), !,
        testSet(Ex, Text, 'are ca solutii', V, Sols)
    ;   failure(Ex, 'INTERNAL: nu avem variabila TheList sau aceasta nu este lista.')
    ).

testSetU(Ex, Text, TypeText, SetG, SetE) :- sort(SetG, SetGUnique),
    testSet(Ex, Text, TypeText, SetGUnique, SetE).
testSet(Ex, Text, TypeText, SetG, SetE) :-
    msort(SetG, SetGSorted), msort(SetE, SetESorted),
    (   SetGSorted == SetESorted, ! ;
        testSetMinus(SetG, SetE, TooMuch),
        testSetMinus(SetE, SetG, TooLittle),
        (   TooMuch == [], TooLittle == [], !,
            M1 = 'vezi duplicate'
        ;   swritef(M1, '%w sunt in plus, %w lipsesc', [TooMuch, TooLittle])
        ),
        swritef(M,
                'Interogarea %w %w %w dar se astepta %w (%w)',
                [Text, TypeText, SetG, SetE, M1]), failure(Ex, M)
    ).

testSetMinus(From, ToRemove, Result) :-
        findall(E, (member(E, From), \+ member(E, ToRemove)), Result).

getVal(Var, [Var=Val | _], Val) :- !.
getVal(Var, [_ | Vars], Val) :- getVal(Var, Vars, Val).
