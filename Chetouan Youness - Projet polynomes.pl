writePoly(0,_) :- !.
writePoly(X,0) :- !, write(X).
writePoly(X,1) :- !, write(X), write('x').
writePoly(X,P) :- write(X), write('x^'), write(P).

afficherReste([[X,P]]) :- write(' + '), writePoly(X,P), !.
afficherReste([[X,P]|L]):- write(' + '), writePoly(X,P), afficherReste(L).

afficher([[X,P]]) :- writePoly(X,P), !.
afficher([[X,P]|L]):- writePoly(X,P), afficherReste(L).

%___________________________________________________________________________________________________

simplifier(List, List_simplifiee) :- group(List, List_groupee), remove_nulls(List_groupee, List_simplifiee).

group([],[]).

group([[Coef, Puissance]|R1],[[Coef, Puissance]|R2]) :- puis_not_in(Puissance, R1), !, group(R1, R2).

group([[Coef, P]|Rest], Output) :- group_Elem([Coef, P], Rest, NewList), group(NewList, Output).

group_Elem([Coef,P], [], [[Coef,P]]).

group_Elem([Coef,P], [[OtherCoef, P]|Rest], NewRest) :- NewCoef is Coef + OtherCoef, group_Elem([NewCoef,P], Rest, NewRest).

group_Elem(P, [[X, Y]|Rest], [[X, Y]|NewRest]):- P\==Y, group_Elem(P, Rest, NewRest).

puis_not_in(_,[]):-!.
puis_not_in(X,[[_,Y]|L]) :- X\==Y, puis_not_in(X,L).

remove_nulls([],[]):-!.
remove_nulls([[0,_]|L], Result) :- !, remove_nulls(L, Result).
remove_nulls([[0.0,_]|L], Result) :- !, remove_nulls(L, Result).
remove_nulls([[X,P]|L1],[[X,P]|L2]) :- remove_nulls(L1,L2).

%___________________________________________________________________________________________________
%evaluer
evaluation([], _, 0).
evaluation([[C,P]|Rest], Value, N):- evaluation(Rest, Value, N2), eval_elem([C, P], Value, N1), N is N1 + N2.

eval_elem([_,P], 0, 0) :- P\==0, !.
eval_elem([N,0], _, N) :- !.
eval_elem([0,_], _,0) :- !.
eval_elem([C,P], V, N) :- N is C * (V**P).
/*
eval_elem([C,P], V, N) :- P>=1, !, N is V * N2, P2 is P - 1, eval_elem([C,P2], V, N2) .
eval_elem([C,P], V, N) :- P<=-1, !,N is N2 9isma V, P2 is P + 1, eval_elem([C,P2], V, N2) .
eval_elem([C,P], V, N) :- P>0, !, .
eval_elem([C,P], V, N) :- P>-1, !.*/

%___________________________________________________________________________________________________

deriver([], []).
deriver([[C,P]|R], [[CD, PD]| R2]) :- deriver_elem([C,P], [CD, PD]), CD\==0, !, deriver(R, R2).
deriver([_|R], R2) :- deriver(R, R2).

deriver_elem([_,0], [0, 0]):- !.
deriver_elem([C,P], [CD, PD]) :- CD is C * P, PD is P - 1.

%___________________________________________________________________________________________________

somme(P1, P2, P3):- append(P1, P2, P4), simplifier(P4, P3).

%___________________________________________________________________________________________________

soustraction(P1, P2, P3) :- negativePol(P2, P4), somme(P1, P4, P3).
negativePol([[X,Y]|R], [[X2,Y]|R2]) :- X2 is X * (-1), negativePol(R, R2).
negativePol([], []).

%___________________________________________________________________________________________________

produit(_, [], []).
produit([], _, []).
produit(P1, [[X,Y]|R], P2):- produit_terme(P1, [X,Y], P3), produit(P1, R, P4), append(P3, P4, P2).

produit_terme([[C,P]|R], [X,Y], [[C2,P2]|R2]) :- C2 is X * C, P2 is P + Y, produit_terme(R, [X,Y], R2).
produit_terme([], _, []).

%___________________________________________________________________________________________________	

division(_, [], [], []):-!.
division([], _, [], []):-!.
division([[C,P]|R], [[_,Y]|_], [], [[C,P]|R]) :- P<Y, !.
division([[C,P]|R], [[X,Y]|R2], [[C2,P2]|R3], R4) :- C2 is C / X, P2 is P - Y, produit([[X,Y]|R2], [[C2,P2]], M),  soustraction([[C,P]|R], M, S),
division(S, [[X,Y]|R2], R3, R4).

%___________________________________________________________________________________________________


:-op(700, fy, [eval]).
:-op(600, fy, [simp,deri]).
:-op(500, yfx, [+,-]).
:-op(400, yfx, [*,/]).
:-op(1200, xfx, est).


isList([]).
isList([[X,Y]|R]) :- number(X), number(Y), isList(R).

fixIt(X,X):- isList(X),!.
fixIt(X,Y):- est(Y,X).

est(P,*(P1, P2)):- fixIt(P1,P11), fixIt(P2,P22), produit(P11, P22, P3), simplifier(P3, P).
est(P,/(P1, P2)):- fixIt(P1,P11), fixIt(P2,P22), division(P11, P22, P, _).
est(P,+(P1, P2)):- fixIt(P1,P11), fixIt(P2,P22), somme(P11, P22, P).
est(P,-(P1, P2)):- fixIt(P1,P11), fixIt(P2,P22), soustraction(P11, P22, P).
est(P, deri(X)):- fixIt(X,Y), deriver(Y, P).
est(P, simp(X)):- fixIt(X,Y), simplifier(Y, P).
est(N,eval(V, P)):- number(V), fixIt(P,P2), evaluation(P2, V, N).


