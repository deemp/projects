:-use_module(library(clpfd)).

%--------------------------
% Agents generation
%--------------------------

start([1,1]).   
    
%used to check distance btw covid and agents(+ start and home)  
at_safe_distance_from(P1, P2) :-
  P1 = [X1,Y1],
  P2 = [X2,Y2],
  Xd is X1-X2,
  Yd is Y1-Y2,
  abs(Xd, X),
  abs(Yd, Y),
    Min is min(X, Y),
    Max is max(X, Y),
    Min >= 0,
    Max >= 2.
  
%tries to find agents' positions. Sometimes fails
produce_agents(N, M, Agents) :- 
    N1 is N+1, 
    M1 is M+1,
  maplist(random(1,N1),[X_doc, X_mask, X_covid_1, X_covid_2, X_home]),
  maplist(random(1,M1),[Y_doc, Y_mask, Y_covid_1, Y_covid_2, Y_home]),
  Doc = [X_doc,Y_doc],
  Mask = [X_mask, Y_mask], 
  Covid1 = [X_covid_1, Y_covid_1], 
  Covid2 = [X_covid_2, Y_covid_2],
  Home = [X_home, Y_home],
  Start = [1,1],
  \+ Home = Start,
  maplist(at_safe_distance_from(Covid1), [Doc, Mask, Home, Start]),
  maplist(at_safe_distance_from(Covid2), [Doc, Mask, Home, Start]),
  Agents = [Doc, Mask, Covid1, Covid2, Home].

%finds some agents configuration. after first agent set is found, cut
get_agents(N,M,Agents) :- 
    (produce_agents(N,M,A), Agents = A; get_agents(N,M,Agents)), !.

%checks if cell coordinate lies on N by M board
on_board(Cell, N, M) :-
    [X, Y] = Cell,
    between(1, N, X),
    between(1, M, Y).

%--------------------------
% Backtracking (DFS)
% slightly modified to reach the goal in acceptable time
%--------------------------

compare_by_distance(X, [D1, _], [D2, _]) :- 
    compare(X, D1, D2).

exists_path(P2, _, _, [P2|Path], [P2|Path], CurLength, Bound) :- CurLength =< Bound.

exists_path(Finish, Dimensions, Agents, [Current|CurPath], FullPath, CurLength, Bound) :-
    \+ Current = Finish,
    CurLength < Bound,
    [Doc, Mask, Covid1, Covid2, [XHome, YHome]] = Agents,
    [N, M] = Dimensions,
    % find accessible vertices with their distances
    findall(
        [Dist, Neighbor], 
        (
            on_board(Neighbor, N, M),
            neighbor_left(Current, Neighbor),
            \+ member(Neighbor, CurPath),
            FullCurPath = [Current | CurPath],
            (   
                member(Mask, FullCurPath);
                member(Doc, FullCurPath);
                maplist(at_safe_distance_from(Neighbor),[Covid1, Covid2])
            ),
            
            Neighbor = [X, Y],
            Xd is X - XHome,
            Yd is Y - YHome,
            abs(Xd, XdAbs),
            abs(Yd, YdAbs),

            % ------Chebyshev distance
            % Dist is max(XdAbs, YdAbs)

            % ------Euclidean distance squared
            Dist is XdAbs*XdAbs + YdAbs*YdAbs
        ), 
        Neighbors
    ),
    
    predsort(compare_by_distance, Neighbors, [[D, Neighbor] | Sorted]),
    % watch process of selecting the path
    % format('~w ~w ~n', [Current, [[D, Neighbor] | Sorted]]),
    NextLength is CurLength + 1,
    exists_path(Finish, Dimensions, Agents, [Neighbor,Current|CurPath], FullPath, NextLength, Bound).


some_path(N,M,Agents,FullPath,Bound) :-
    [_, _, _, _, Home] = Agents,
    Start = [1,1],
    exists_path(Home, [N, M], Agents, [Start], FullPath, 1, Bound).

%for-loop. increase current bound until solution is found
shortest_path(N,M,Agents,FullPath,CurBound,Bound) :-
    MaxBound is N*M,
    CurBound < MaxBound,
    (
        some_path(N,M,Agents,FullPath,CurBound),
        Bound = CurBound
        ;
        NextBound is CurBound + 1,
        shortest_path(N,M,Agents,FullPath,NextBound,Bound)
    ).
    
shortest_path(N,M,Agents,Path) :-
    shortest_path(N,M,Agents,FullPath,0,_),
    reverse(FullPath,Path).

dfs(Dimensions,Agents,ShortestPath) :-
    Dimensions = [N, M],
    shortest_path(N,M,Agents,ShortestPath)
    ;
    ShortestPath = [].

%--------------------------
% BFS
%--------------------------


neighbor_left(A,B) :-
    A = [X1, Y1],
    B = [X2, Y2],
    X1_p is X1+1,
    X1_n is X1-1,
    Y1_p is Y1+1,
    Y1_n is Y1-1,
    (
        member(X2,[X1_p,X1_n]), member(Y2,[Y1,Y1_p,Y1_n])
        ;
        X1 = X2, member(Y2,[Y1_p,Y1_n])
    ).

neighbor_right(A,B) :-
    neighbor_left(B, A).
    
member_of(List, Term) :-
    member(Term, List).

%--------------------------
% unprotected BFS
% Assumes any path doesn't go through Mask or Doc
% Hence, there is no protection from Covid
%--------------------------

bfs_unprotected(Finish,_,[Finish|_],_,_,AllParents,AllParents).
bfs_unprotected(Finish,Dimensions,[F|Queue],Visited,Agents,CurParents,AllParents) :- 
    Agents = [Doc, Mask, Covid1, Covid2, _],
    Dimensions = [N, M],
    findall(
        Neighbor,
        (
        neighbor_left(F,Neighbor),
        on_board(Neighbor,N,M),
        \+ member(Neighbor, Visited),
        (
            \+ member(Neighbor, [Doc, Mask]) ;
            Neighbor = Finish 
        ),
        maplist(at_safe_distance_from(Neighbor), [Covid1, Covid2])
        ), 
        Neighbors),
    append(Visited,Neighbors,VisitedMore),
    findall([F,Y],member(Y,Neighbors),ParentsNew),
    append(CurParents,ParentsNew,CurParentsMore),
    append(Queue,Neighbors,QueueMore),
    (
        member(Finish,Neighbors), AllParents = CurParentsMore;
        bfs_unprotected(Finish,Dimensions,QueueMore,VisitedMore,Agents,CurParentsMore,AllParents)
    ).

run_bfs_unprotected(Start,Finish,Dimensions,Agents,AllParents) :-
    bfs_unprotected(Finish,Dimensions,[Start],[Start],Agents,[],AllParents).

% get path from Start to Finish knowing all parents
get_path(Start,Start,_,Path,Path).

get_path(Start,Finish,[[Parent,Finish]|AllParents],CurPath,Path) :-
    get_path(Start,Parent,AllParents,[Parent|CurPath],Path).

get_path(Start,Finish,[[_,Y]|AllParents],CurPath,Path) :-
    \+ Y = Finish,
    get_path(Start,Finish,AllParents,CurPath,Path).
    
get_path(Start,Finish,AllParents,Path) :- 
    get_path(Start,Finish,AllParents,[Finish],Path).

path_bfs_unprotected(Start,Finish,Dimensions,Agents,Path) :-
    run_bfs_unprotected(Start, Finish, Dimensions, Agents, AllParents),
    reverse(AllParents, AllParentsReversed),
    get_path(Start,Finish,AllParentsReversed,Path)
    ;
    Path = [].
% path_bfs_unprotected([1,1],[5,1],[10,10],[[5, 4], [5, 4], [3, 2], [3, 5], [5, 1]],Path).

%--------------------------
% protected BFS
% Assumes any path goes through Mask or Doc
% Hence, there is always a protection from Covid
%--------------------------

bfs_protected(Finish,_,[Finish|_],_,_,AllParents,AllParents).
bfs_protected(Finish,Dimensions,[F|Queue],Visited,Agents,CurParents,AllParents) :- 
    Dimensions = [N, M],
    findall(
        Neighbor,
        (
            neighbor_left(F,Neighbor),
            on_board(Neighbor,N,M),
            \+ member(Neighbor, Visited)
        ), 
        Neighbors),
    append(Visited,Neighbors,VisitedMore),
    findall([F,Y],member(Y,Neighbors),ParentsNew),
    append(CurParents,ParentsNew,CurParentsMore),
    append(Queue,Neighbors,QueueMore),
    (
        member(Finish,Neighbors), AllParents = CurParentsMore
        ;
        bfs_protected(Finish,Dimensions,QueueMore,VisitedMore,Agents,CurParentsMore,AllParents)
    ).

run_bfs_protected(Start,Finish,Dimensions,Agents,AllParents) :-
    bfs_protected(Finish,Dimensions,[Start],[Start],Agents,[],AllParents).

path_bfs_protected(Start,Finish,Dimensions,Agents,Path) :-
    run_bfs_protected(Start, Finish, Dimensions, Agents, AllParents),
    reverse(AllParents, AllParentsReversed),
    get_path(Start,Finish,AllParentsReversed,Path).

% path_bfs_protected([1,1],[5,1],[10,10],[[5, 4], [5, 4], [3, 2], [3, 5], [5, 1]],Path).

%--------------------------
% BFS
% Chooses minimal path to Home suggested by 
%   unprotected BFS
%   unprotected BFS to Doc or Mask + protected BFS to Home
%--------------------------

%https://www.swi-prolog.org/pldoc/man?predicate=keysort/2

:-use_module(library(pairs)).

bfs(Dimensions,Agents,ShortestPath) :-
    Agents = [Doc, Mask, _, _, Home],
    start(Start),
    path_bfs_unprotected(Start,Home,Dimensions,Agents,PathUnprotected),
    path_bfs_unprotected(Start,Doc,Dimensions,Agents,PathToDoc),
    path_bfs_unprotected(Start,Mask,Dimensions,Agents,PathToMask),
    path_bfs_protected(Doc,Home,Dimensions,Agents,[DocCell|PathFromDoc]),
    path_bfs_protected(Mask,Home,Dimensions,Agents,[MaskCell|PathFromMask]),
    (
        PathToDoc = [] -> PathThroughDoc = []
        ;
        append(PathToDoc,PathFromDoc,PathThroughDoc)
    ),
    (
        PathToMask = [] -> PathThroughMask = []
        ;
        append(PathToMask,PathFromMask,PathThroughMask)
    ),
    map_list_to_pairs(length, [PathUnprotected,PathThroughDoc,PathThroughMask], Pairs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, SortedValues),
    (
        delete(SortedValues, [], [ShortestPath | SortedNoEmpty]);
        ShortestPath = []
    ), !.

        
%--------------------------
% Printing
%--------------------------


run_n_times_methods(I, Times, Dimensions) :-
    I1 is I + 1,
    I1 < Times,
    Dimensions = [N, M],
    get_agents(N, M, Agents), 

    get_time(StartDfs),
    call(dfs, Dimensions, Agents, _),
    get_time(FinishDfs),
    ExecutionTimeDfs is (FinishDfs - StartDfs)*10000,
    
    get_time(StartBfs),
    call(bfs, Dimensions, Agents, _),
    get_time(FinishBfs),
    ExecutionTimeBfs is (FinishBfs - StartBfs)*1000,
    
    format('~3f,~3f~n', [ExecutionTimeDfs, ExecutionTimeBfs]),
    run_n_times_methods(I1, Times, Dimensions)
    ;
    true.

% outputs comparison table of running times of 2 algorithms, in msec
run_n_times_methods(Times, Dimensions) :-
    writeln("DFS     BFS"),
    run_n_times_methods(0, Times, Dimensions).

% Example:
% run_n_times_methods(10, [10, 10]).

print_field(Cur, Dimensions, Agents, ShortestPath) :-
    Cur = [X, Y],
    Dimensions = [N, M],
    Y = 0,
    X = 1.
    
print_field(Cur, Dimensions, Agents, ShortestPath) :-
    Agents = [Doc, Mask, Covid1, Covid2, Home],
    (
        (Cur = Home) -> write('h');
        start(Cur) -> write('s');
        (Cur = Doc) -> (member(Cur, ShortestPath) -> write('d'); write('D'));
        (Cur = Mask) -> (member(Cur, ShortestPath) -> write('m'); write('M'));
        %C for Covid
        member(Cur, [Covid1, Covid2]) -> (member(Cur, ShortestPath) -> write('c'); write('C'));
        %I for Infected
        neighbor_right(Cur, Covid1) -> (member(Cur, ShortestPath) -> write('i'); write('I'));
        neighbor_right(Cur, Covid2) -> (member(Cur, ShortestPath) -> write('i'); write('I'));
        member(Cur, ShortestPath) -> write('+');
        write('.')
    ),
    Cur = [X,Y],
    Dimensions = [N, M],
    (
        X = N, write('\n'), 
        Y1 is Y - 1,
        X1 is 1,
        print_field([X1, Y1], Dimensions, Agents, ShortestPath)
        ;
        X2 is X + 1,
        print_field([X2, Y], Dimensions, Agents, ShortestPath)
    ).

print_field(Method, Dimensions, Agents, ShortestPath) :- 
    call(Method, Dimensions, Agents, ShortestPath),
    Dimensions = [N,M],
    print_field([1,M], Dimensions, Agents,ShortestPath),
    (
        ShortestPath = [], writeln("Lose")
        ;
        writeln("Win")
    ).

% Displays results of running a particular method
run(Method, Dimensions, Agents) :- 
    get_time(Start),
    call(Method, Dimensions, Agents, ShortestPath),
    get_time(Finish),
    ExecutionTime is Finish - Start,
    Dimensions = [N,M],
    print_field([1,M], Dimensions, Agents,ShortestPath),
    (
        ShortestPath = [], writeln("Lose")
        ;
        length(ShortestPath, L),
        format('Win ~nSolution found in ~w (ms) ~nLength of Path: ~w ~nPath: ~w', [ExecutionTime, L, ShortestPath])
    ).

% Generates random configuration and runs chosen method
% Test program with it
random_run(Method, N, M) :-
    get_agents(N, M, Agents),
    run(Method,[N,M],Agents).
% Example:

% random_run(bfs, 10, 10).

% can be dfs instead of bfs
% N and M are board sizes
% Agents = [Doc, Mask, Covid1, Covid2, Home]

%--------------------------
% "Impossible" maps
%--------------------------

% 1)
% run(bfs,[10,10],[[5, 5], [2, 9], [2, 3], [4, 1], [1, 2]]).
% ..........
% .M........
% ..........
% ..........
% ..........
% ....D.....
% III.......
% ICI.......
% hIIII.....
% s.ICI.....

% 2)
% run(bfs,[10,10],[[5, 5], [2, 9], [1, 3], [3, 2], [1, 2]]).
% ..........
% .M........
% ..........
% ..........
% ..........
% ....D.....
% II........
% CIII......
% hICI......
% sIII......

% 3) 
% run(bfs,[10,10],[[9, 10], [9, 10], [10, 8], [7, 10], [10, 10]]).
% .....ICIDh
% .....IIIII
% ........IC
% ........II
% ..........
% ..........
% ..........
% ..........
% ..........
% s.........

