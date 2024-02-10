% mammals

warm_blooded(penguin).
warm_blooded(human).
produce_milk(penguin).
produce_milk(human).
have_feather(penguin).
have_hair(human).

mammal(X) :-
	warm_blooded(X), 
	produce_milk(X), 
	have_hair(X).

% parents

parent('James I', 'Charles I').
parent('James I', 'Elizabeth').
parent('Charles I', 'Catherine').
parent('Charles I', 'Charles I').
parent('Charles I', 'Charles II').
parent('Elizabeth', 'Sophia').
parent('Sophia', 'George I').

maplist(male, ['James I', 'Charles I']).

mother(M, X) :-
	parent(M, X), 
	female(M).

% game

game(1,0,b).
game(0,1,w).

game(B, W, L) :-
	B > 0, B1 is B - 1, 
	game(B1, W, L).

game(B, W, L) :-
	W > 1, B1 is B + 1, W1 is W - 2, 
	game(B1, W1, L).

gcd(X, Y, R) :-
	Y > 0, X >= Y, X1 is X - Y, 
	gcd(X1, Y, R).

gcd(X, Y, R) :-
	X < Y, 
	gcd(Y, X, R).

gcd(X, 0, X) :-
	X > 0.