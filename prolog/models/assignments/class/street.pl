color(blue).
color(green).
color(red).
color(white).
color(yellow).

nationality(dane).
nationality(englishman).
nationality(german).
nationality(swede).
nationality(norwegian).

drink(beer).
drink(coffee).
drink(milk).
drink(tea).
drink(water).

cigarette(blend).
cigarette(blueMaster).
cigarette(dunhill).
cigarette(pallMall).
cigarette(prince).

pet(birds).
pet(cats).
pet(dogs).
pet(fish).
pet(horses).

index(1).
index(2).
index(3).
index(4).
index(5).

check(H, N, D, C, P, I) :-
	color(H), 
	nationality(N), 
	drink(D), 
	cigarette(C), 
	pet(P), 
	index(I)
    .

man(red, englishman, D, C, P, I) :-
	check(red, englishman, D, C, P, I).

man(H, swede, D, C, dogs, I) :-
	check(H, swede, D, C, dogs, I).

man(H, dane, tea, C, P, I) :-
	check(H, dane, tea, C, P, I).

man(green, N, D, C, P, I) :-
	check(green, N, D, C, P, I), 
    N \= N1,
    D \= D1,
    C \= C1,
    P \= P1, 
    I1 is I + 1, 
	man(white, N1, D1, C1, P1, I1)
    . 

man(green, N, coffee, C, P, I) :-
	check(green, N, coffee, C, P, I)
    .

man(H, N, D, pallMall, birds, I) :-
	check(H, N, D, pallMall, birds, I)
    .

man(yellow, N, D, dunhill, P, I) :-
	check(yellow, N, D, dunhill, P, I)
    .

man(H, N, milk, C, P, 3) :-
	check(H, N, milk, C, P, 3)
    .

man(H, norwegian, D, C, P, 1) :-
	check(H, norwegian, D, C, P, 1)
    .

man(H, N, D, blend, P, I) :-
	check(H, N, D, blend, P, I),
    H \= H1,
    N \= N1,
    D \= D1,
    P \= cat,
    C1 \= blend,
	(I1 is I - 1; I1 is I + 1),
    man(H1, N1, D1, C1, cat, I1)
    . 

man(H, N, beer, blueMaster, P, I) :-
	check(H, N, beer, blueMaster, P, I)
    .

man(H, N, D, dunhill, P, I) :-
	check(H, N, D, dunhill, P, I), 
    H \= H1, 
    N \= N1, 
    D \= D1, 
    P \= horses, 
    C1 \= dunhill,
    (I1 is I - 1; I1 is I + 1),
    man(H1, N1, D1, C1, horses, I1)
    . 

man(H, german, D, prince, P, I) :-
	check(H, german, D, prince, P, I)
    .

man(H, norwegian, D, C, P, I) :-
	check(H, norwegian, D, C, P, I), 
    H \= blue, 
    N1 \= norwegian, 
    D \= D1, 
    C \= C1, 
    P \= P1, 
	(I1 is I - 1; I1 is I + 1),
    man(blue, N1, D1, C1, P1, I1)
    . 

man(H, N, D, blend, P, I) :-
	check(H, N, D, blend, P, I), 
    H \= H1, 
    N \= N1, 
    D \= water, 
    C1 \= blend, 
    P \= P1, 
	(I1 is I - 1; I1 is I + 1),
    man(H1, N1, water, C1, P1, I1)
    . 

solve(X) :-
    X = [M1, M2, M3, M4, M5],
    M1 = [H1, N1, D1, C1, P1, I1],
    M2 = [H2, N2, D2, C2, P2, I2],
    M3 = [H3, N3, D3, C3, P3, I3],
    M4 = [H4, N4, D4, C4, P4, I4],
    M5 = [H5, N5, D5, C5, P5, I5],
    
    man(H1, N1, D1, C1, P1, I1),
	man(H2, N2, D2, C2, P2, I2), 
	man(H3, N3, D3, C3, P3, I3), 
	man(H4, N4, D4, C4, P4, I4), 
	man(H5, N5, D5, C5, P5, I5)
    . 