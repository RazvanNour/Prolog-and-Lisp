aparitii([],_,0).
aparitii([E|T],E,R):-
    aparitii(T,E,RC),
    R is RC+1.
aparitii([_|T],E,R):-
    aparitii(T,E,R).

inv(0, C, C).
inv(E, C, R) :- E =\= 0,
    UC is E mod 10,
    NE is (E - UC)/ 10,
    NC is C * 10 + UC,
    inv(NE, NC, R).


exista([E|_],E):-!.
exista([_|T],E):-
    exista(T,E).



diferenta([],_,[]).
diferenta([H|T],L,R):-
    exista(L,H),
    diferenta(T,L,R).
diferenta([H|T],L,[H|R]):-
    diferenta(T,L,R).


reuniune([],L,L).
reuniune([H|T],L,R):-
    exista(L,H),
    reuniune(T,L,R).
reuniune([H|T],L,[H|R]):-
    reuniune(T,L,R).

bbb([],[]).
bbb([H|T],[H,1|R]):-
    mod(H,2)=:=0,
    bbb(T,R).
bbb([H|T],[H|R]):-
    bbb(T,R).


sterge([],_,[]).
sterge([E|T],E,R):-
    sterge(T,E,R).
sterge([H|T],E,[H|R]):-
    sterge(T,E,R).

pctb([],[]).
pctb([H|T],[[H,X]|R]):-
    aparitii(T,H,E),
    X is E+1,
    sterge(T,H,Q),
    pctb(Q,R).


tema([],[]).
tema([H|T],R):-
    aparitii(T,H,X),
    X>0,
    tema(T,R).
tema([H|T],[H|R]):-
    tema(T,R).

gcd(0,B,B):-!.
gcd(A,0,A):-!.
gcd(A,B,R):-
    A>=B,
    A1 is A-B,
    gcd(A1,B,R),!.
gcd(A,B,R):-
    A=<B,
    B1 is B-A,
    gcd(A,B1,R).

mare([],_).
mare([H1,H2|_],X):-
    gcd(H1,H2,X),
    mare([],X).
mare([H1,H2|T],R):-
    gcd(H1,H2,H),
    mare([H|T],R).

inserare([],E,[E]).
inserare([H|T],E,[E,H|T]):-
    H>E.
inserare([H|T],E,[H|R]):-
    H=<E,
    inserare(T,E,R).


sortare([],C,C).
sortare([H|T],C,R):-
    inserare(C,H,Q),
    sortare(T,Q,R).

ex8([],_,_,_,[]).
ex8([H|T],E,C,N,[H|R]):-
    C=\=0,
    C1 is C-1,
    ex8(T,E,C1,N,R).
ex8([H|T],E,C,N,[E,H|R]):-
    C is 0,
    N1 is N*2,
    C1 is N-1,
    ex8(T,E,C1,N1,R).



maxx([X],X):-!,true.
maxx([H|T],H):-
    maxx(T,M),
    H>M.
maxx([H|T],M):-
    maxx(T,M),
    H=<M.

cincia([],[],_,_).
cincia([H|T],[C|R],C,M):-
    maxx(M,Q),
    H =:= Q,
    C1 is C+1,
    cincia(T,R,C1,M).
cincia([H|T],R,C,M):-
    maxx(M,Q),
    H=\=Q,
    C1 is C+1,
    cincia(T,R,C1,M).


pctbb([],[]).
pctbb([H|T],[W|R]):-is_list(H), !,
    cincia(H,W,1,H),
    pctbb(T,R).
pctbb([H|T],[H|R]):-
    pctbb(T,R).


plusunu([],C,[1]):-C=:=1.
plusunu([],_,[]).
plusunu([H|T],C,[Q|R]):-
    C =:= 1,
    H =:= 9,
    Q is 0,
    plusunu(T,1,R).
plusunu([H|T],C,[Q|R]):-
    C =:= 1,
    Q is H+1,
    plusunu(T,0,R).
plusunu([H|T],C,[H|R]):-
    C=:=0,
    plusunu(T,0,R).


f([],0).
f([H|T],S):-f(T,S1),S1>2,S = S1+H.
f([_|T],S):-f(T,S1),S=S1+1.


invers([],C,C).
invers([H|T],C,R):-
    invers(T,C,[H|R]).

nrocc([],_,0).
nrocc([H|T],E,C1):-
    E=:=H,
    C is C1+1,
    nrocc(T,E,C).
nrocc([H|T],E,C):-
    H=\=E,
    nrocc(T,E,C).


inserae([], E, [E]).
inserae([H|T], E, [E,H|T]).
inserae([H|T], E, [H|R]) :-
         inserae(T, E, R).

permutari([], []).
permutari([H|T], R) :-
    permutari(T, RP),
    inserae(RP, H, R).

allsoltions(L, R) :-
    findall(RPartial, permutari(L, RPartial), R).


combinari(_, 0, []).
combinari([H|T], K, [H|R]) :-
    K > 0,
    NK is K - 1,
    combinari(T, NK, R).
combinari([_|T], K, R) :-
    K > 0,
    combinari(T, K, R).


allsolutions(L, N, R) :-
    findall(RPartial, combinari(L, N, RPartial), R).



aranjamente(L, K, R) :-
    combinari(L, K, RC),
    permutari(RC, R).

subsett([], []).
subsett([_|T], R) :-
	subsett(T, R).
subsett([H|T], [H|R]) :-
	subsett(T, R).

subsetsum([], 0, []).
subsetsum([_|T], S, R) :-
	subsetsum(T, S, R).
subsetsum([H|T], S, [H|R]) :-
	H =< S,
	S1 is S - H,
	subsetsum(T, S1, R).

candidat([H|_], H).
candidat([_|T], Rez) :-
    candidat(T, Rez).




% 15. For a given n, positive, determine all decomposition of n as a sum of consecutive natural numbers.

% onesolution(n, e) =
%	[], n = 0
%	e + onesolution(n - e, e + 1), otherwise

% onesolution(N:number, E:number, R:list)
% onesolution(i, i, o)
%
%
onesolution(0, _, []).
onesolution(N, E, [E|R]) :- N >= E,
    NN is N - E,
    NE is E + 1,
    onesolution(NN, NE, R).

% decomposition(n, e) =
%	onesolution(n, e), e < n
%	onesolution(n, e + 1), e < n

% decomposition(N:number, E:number, R:list)
% decomposition(i, i, o)

decomposition(N, E, R) :- E < N,
    onesolution(N, E, R).
decomposition(N, E, R) :- E < N,
    NE is E + 1,
    decomposition(N, NE, R).


% allsolutions(N:number, R:list)
% allsolutions(i, o)

allsolutions(N, R) :-
    findall(RPartial, decomposition(N, 1, RPartial), R).


produs([],C,C).
produs([H|T],C1,R):-
    C is C1*H,
    produs(T,C,R).

finaal(L,K,P,R1):-
    aranjamente(L,K,R1),
    produs(R1,1,P2),
    P2=P.

findfinal(L,K,P,R):-
    findall(R1,finaal(L,K,P,R1),R).


putere(_,0,C,C).
putere(X,N1,C1,R):-
    N is N1-1,
    C is C1*X,
    putere(X,N,C,R).




inser([],E,[E]).
inser([H|T],E,[E,H|T]).
inser([H|T],E,[H|R]):-
    inser(T,E,R).

perm([],[]).
perm([H|T],R):-
    perm(T,R1),
    inser(R1,H,R).

ver([],_,C,C).
ver([H|T],X,C,R):-
    X=:=1000,
    ver(T,H,C,R).
ver([H|T],X,C,R):-
    abs(X-H)>3,
    C1 is C+1,
    ver(T,H,C1,R).
ver([H|T],X,C,R):-
    abs(X-H)<4,
    ver(T,H,C,R).

fuu(L,R):-
    perm(L,R),
    ver(R,1000,0,Q),
    Q=:=0.

findtot(L,R):-
    findall(REZ,fuu(L,REZ),R).


summ([],C,C).
summ([H|T],C,R):-
    C1 is C+H,
    summ(T,C1,R).

inse([],E,[E]).
inse([H|T],E,[E,H|T]).
inse([H|T],E,[H|R]):-
    inse(T,E,R).

per([H|T],R):-
    per(T,R1),
    inse(R1,H,R).

nrEl([],C,C).
nrEl([_|T],C,R):-
    C1 is C+1,
    nrEl(T,C1,R).


nrE([],0).
nrE([_|T],R1):-
    nrE(T,R),
    R1 is R+1.

p2(L,0,L).
p2([],_,[]).
p2([H|T],N,R):-
    is_list(H),
    nrEl(H,0,R1),
    R1 mod 2=:=0,
    N1 is N-1,
    p2(T,N1,R).
p2([H|T],N,[H|R]):-
    p2(T,N,R).
















