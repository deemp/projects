run_step(IsEvenM, Step, In, Out) :-
    [A,B,C] = In,
    [A1,B1,C1] = Out,
	T is mod(Step, 3),
	(
        (T = 0) -> (IsEvenM -> (O1 = A, O2 = B); (O1 = A, O2 = C));
        (T = 1) -> (IsEvenM -> (O1 = A, O2 = C); (O1 = A, O2 = B));
        (T = 2) -> (O1 = B, O2 = C)
    ),
    (
        (O1 = [H1|T1], O2 = [H2|T2]) -> (
            (H1 < H2) -> (O1_ = T1, O2_ = [H1,H2|T2]);
            (O1_ = [H2,H1|T1], O2_ = T2)
        );
        (O1 = [H1|T1]) -> (O1_ = T1, O2_ = [H1|O2]);
        (O2 = [H2|T2]) -> (O1_ = [H2|O1], O2_ = T2);
        O1_ = O1, O2_ = O2
    ),
    (
        (T = 0) -> (IsEvenM -> (A1 = O1_, B1 = O2_, C1 = C); (A1 = O1_, B1 = B, C1 = O2_));
        (T = 1) -> (IsEvenM -> (A1 = O1_, B1 = B, C1 = O2_); (A1 = O1_, B1 = O2_, C1 = C));
        (T = 2) -> (A1 = A, B1 = O1_, C1 = O2_)
    )
    . 
    
run_steps(_, N, N, In, States, [In|States]).
run_steps(IsEvenM, N, StepCur, In, StatesCur, States) :- 
    run_step(IsEvenM, StepCur, In, Out),
    NextStep is StepCur + 1,
    run_steps(IsEvenM, N, NextStep, Out, [In|StatesCur], States)
    . 

run(M, States) :-
    IsEvenM = (0 is mod(M, 2)),
    N is (2 ** M) - 1,
    numlist(1,M,L),
    run_steps(IsEvenM, N, 0, [L, [], []], [], StatesSolution),
    reverse(StatesSolution, StatesReversed),
    maplist(maplist(reverse), StatesReversed, States)
    . 

show_state(Step, State) :-
    maplist(maplist(number_string), State, Strings),
    maplist(atomic_list_concat, Strings, StringsConcatenated),
    format('State ~d:~n|~w~n|~w~n|~w~n~n', [Step|StringsConcatenated])
    . 
    
show_states(M) :-
    run(M, States),
    length(States, StatesLen),
    T is StatesLen - 1,
    numlist(0, T, L),
    maplist(show_state, L, States)
    .

main(NumberOfDisks) :- 
    (NumberOfDisks > 0) -> show_states(NumberOfDisks)
    . 

