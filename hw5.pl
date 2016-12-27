/* ECS 140 - hw5.pl */

consult('part1Facts.pl').
consult('test.pl').

/* Part 1: Simple Queries*/

% Query 1: Find all the names of the novels published in either 1953 or 1996.
year_1953_1996_novels(N) :- 
	novel(N, 1996) ; novel(N, 1953).


% Query 2: Find names of all novels published between 1800 and 1900.
period_1800_1900_novels(N) :-
	novel(N, Y), 
	Y >= 1800, Y =< 1900.

% Query 3: Find all names of the characters who are fans of LOTR.
lotr_fans(F) :-
	fan(F, L),
	member(the_lord_of_the_rings, L). 
	
% Query 4: Find all the names of the authors whose novels chandler is a fan of. 
author_names(A) :-
	fan(chandler, L1),
	member(N, L1),
	author(A, L2),
	member(N, L2).
	

% Query 5: Find all the names of the characters who are fans of the novels authored by brandon sanderson.
fans_names(F) :-
	author(brandon_sanderson, L1),
	member(N, L1),
	fan(F, L2),
	member(N, L2).
	

% Query 6: Find all the names of the novels that are common between either of phoebe, ross, and monica. (PR, PM, or RM)
mutual_novels(N) :-
	% novels common between phoebe and ross
	fan(phoebe, PL),
	member(N, PL),
	fan(ross, RL),
	member(N, RL) ;
	
	% novels common between phoebe and monica
	fan(phoebe, PL),
	member(N, PL),
	fan(monica, ML),
	member(N, ML) ;
	
	% novels common between ross and monica
	fan(ross, RL),
	member(N, RL),
	fan(monica, ML),
	member(N, ML).
	
	
/* Part 2: List Manipulation Predicates */

% 1) return whether element X is a member of set Y
isMember(X, [X|_]). 
isMember(X, [_|T]) :-
	isMember(X, T). 

	
% 2) find Z, the union of X and Y
isUnion([], Y, Y).
isUnion([H|T], Y, Z) :-
	isMember(H, Y),
	!,
	isUnion(T, Y, Z).
isUnion([H|T1], Y, [H|T2]) :-
	isUnion(T1, Y, T2).

	
% 3) find Z, the intersection of X and Y
isIntersection([], Y, []).
isIntersection([H|T1], Y, [H|T2]) :-
	isMember(H, Y),
	!,
	isIntersection(T1, Y, T2).
isIntersection([H|T], Y, Z) :-
	isIntersection(T, Y, Z).

	
% 4) return whether X and Y are equal
% isEqual(X, X).
isEqual(X, Y) :-
	sub(X, Y, []),
	sub(Y, X, []).

sub([], Y, []).
sub([H|T], Y, Z) :-
	isMember(H, Y),
	!,
	sub(T, Y, Z).
sub([H|T1], Y, [H|T2]) :-
	sub(T1, Y, T2).	
	
% 5) find Y, the powerset of X
subset([], []).
subset([_|T], Y) :-
	subset(T, Y).
subset([H|T1], [H|T2]) :-
	subset(T1, T2).

powerSet(X, Y) :-
	setof(Y, subset(X, Y), Y).
  
  
/* Part 3: Puzzle */ 

%% A farmer must ferry a wolf, a goat, and a cabbage across a river using a boat that is too small to take more than one of the three across at once. 
%% If he leaves the wolf and the goat together, the wolf will eat the goat, and if he leaves the goat with the cabbage, the goat will eat the cabbage. 
%% How can he get all three across the river safely? (take goat there -> go back -> take wolf tere -> take goat back and drop him off -> take cabbage there -> go back -> take goat there)

% declare left and right banks
opposite(left, right).
opposite(right, left).

% initial state: goat is alone on the right bank
state(left, left, right, left).
		
% define safe and unsafe states
unsafe(state(F, W, G, C)) :- opposite(F, W), opposite(F, G) ; opposite(F, G), opposite(F, C).
safe(A) :- \+ unsafe(A).

% X is an object (wolf/ cabbage/ goat)
take(X, A, B) :- opposite(A, B).

% arc returns true if move N (take object) takes state X to state Y

% farmer moves between banks without taking any object
arc(take(none, S1, S2), state(S1, W, G, C), state(S2, W, G, C)) :-
	take(none, S1, S2).

% farmer takes goat	
arc(take(goat, S1, S2), state(S1, W, S1, C), state(S2, W, S2, C)) :-
	take(goat, S1, S2).
	
% farmer takes wolf 
arc(take(wolf, S1, S2), state(S1, S1, G, C), state(S2, S2, G, C)) :-
	take(wolf, S1, S2).

% farmer takes cabbage
arc(take(cabbage, S1, S2), state(S1, W, G, S1), state(S2, W, G, S2)) :-
	take(cabbage, S1, S2).
	
% - Check what objects have been dropped off already
traverse(State1, State2, Visited) :-
	LegalMove = arc(_, State1, State2),
	\+ isMember(LegalMove, Visited),
	LegalMove,
	safe(State1),
	printvisits([LegalMove|Visited]), !.
	
traverse(State1, State2, Visited) :-
	LegalMove = arc(take(_,_,_), State1, NextState),
	\+ isMember(LegalMove, Visited),
	LegalMove,
	safe(State1),
	traverse(NextState, State2, [LegalMove|Visited]).

% solve puzzle	
solve :- 
	traverse(state(left, left, left, left), state(right, right, right, right), []), !.

% completed takes
printvisits([]).
printvisits([arc(take(X, A, B), _, State2)|T]) :-
	printvisits(T),
	print('take('), print(X), print(','), print(A), print(','), print(B), print(')'), nl.