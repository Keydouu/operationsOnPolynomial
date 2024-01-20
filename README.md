polynomials are written as arrays, for example the fellowing array

[ [1.5, 0] , [-1,1] , [0, 2] , [3.5,3] ].

refer to 3.5 x^3 + 0 * x^2 - x + 1.5

some examples to try the code :

?- P est [ [1, 0] , [-1,1] , [0, 2] , [2,3] ] + [ [-1, 0] , [2,1] , [3, 2] ]

P = [ [0, 0], [1, 1], [3, 2], [2, 3]]

?- V est eval(2, [ [0, 0], [1, 1], [3, 2], [2, 3]]).

V = 30

?- P est [[2,2],[1,1]] / [[4,1],[2,0]].

P = [[0.5,1]]
