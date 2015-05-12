fc_course(C):-
	course(C,_,N), (N=3; N=4).
prereq_110(C):-
	course(C,X,_), member(ecs110,X).
ecs140a_students(N):-
	student(N,X), member(ecs140a,X).
	
instructor_names(I):-
	instructor(I,_), teach_john(N), member(I,N).
teach_john(I):-
	findall(P, (instructor(P,C),student(john,L),member(X, C),member(X,L), (C \=L)), U), sort(U, I).
students(N):-
	student(N,_), with_jim(S), member(N, S).	
with_jim(S):-
	findall(P, (student(P,C),instructor(jim,L),member(X, C),member(X,L), (C \=L)), U), sort(U, S).

allprereq(C,PS):-
	findall(CC, (course(CC,_,_),prereq(C,CC)), PS).
prereq(C,P):- 
	course(C, Pre, _), member(P, Pre); 
	course(C, Pre, _), member(T, Pre), Pre\=[], prereq(T,P).	

all_length([H|T],L):-
	atom(H), all_length(T, L2),!, L is 1+L2; 
	all_length(H,L1), all_length(T,L2),!, L is L1+L2.
all_length([],0):-
	!.

equal_a_b(L):-
	equal_help(L,0,0).
equal_help([], A, A):-
	!.
equal_help([H|T], Acount, Bcount):-
	atom(H), H == a, AC is Acount+1,!, equal_help(T, AC, Bcount);
	atom(H), H == b, BC is Bcount+1,!, equal_help(T, Acount, BC); 
	!, equal_help(T, Acount,Bcount);
	Acount =:= Bcount.

swap_prefix_suffix(K, L, S) :-
    append3(L1,K,L3,L), append3(L3,K,L1,S).
append3(L1,L2,L3,L) :-
    append(L1,LL,L),append(L2,L3,LL).

palin(L):-
	my_reverse(L, LL), L == LL.
my_reverse(L, RL):-
	my_reverse(L, RL, []).
my_reverse([], LR, LR):-
	!.
my_reverse([H|T], LU, LR) :-
    my_reverse(T, LU, [H|LR]).
	
good(A):-
	check(A).
check([0]):-
	!.
check([H|T]):-
	H == 1, append3([1], [0], L, [H|T]), check(L);
	H == 0, check(T).

not(A):-
	A, !, fail;
	!.
  	
solve:- 
	go(state(left, left, left, left), state(right, right, right, right)).

go(Start, Goal):-
	path(Start, Goal, [Start]).
	
path(Start, Goal, Repeat):- 
	move(Start, Next), not(member(Next, Repeat)),! , Next \= Goal, !, path(Next, Goal, [Next|Repeat]), !.
	
path(Goal, Goal, P):-
	!.
	 
opposite(left, right):-
	!.
opposite(right, left):-
	!. 

unsafe(state(A, B, B, C)):-
	opposite(A, B), !.
unsafe(state(A, C, B, B)):-
	opposite(A, B), !.
	
safe(not(unsafe(A))):-
	!.

member(H, [H|_]):-
	!.
member(H, [_T]):-
	member(H,T), !.

move(state(A, W, G, C), state(B, W, G, C)):-
	opposite(A, B), not(unsafe(state(B, W, G, C))), write('take(none,'), write(A), write(','), write(B), write(')'), nl.
	
move(state(A, W, G, A), state(B, W, G, B)):-
	opposite(A, B), not(unsafe(state(B, W, G, B))), write('take(cabbage,'), write(A), write(','), write(B), write(')'), nl.	
	

	
move(state(A, A, G, C), state(B, B, G, C)):-
	opposite(A, B), not(unsafe(state(B, B, G, C))),  write('take(wolf,'), write(A), write(','), write(B), write(')'), nl.
	
move(state(A, W, A, C), state(B, W, B, C)):-
	opposite(A, B), not(unsafe(state(B, W, B, C))), write('take(goat,'), write(A), write(','), write(B), write(')'), nl.	




