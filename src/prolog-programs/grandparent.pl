parent(joe, bob).
parent(joe, steve).
parent(steve, casy).
parent(casy, alex).
parent(steve, stacy).

grandparent(X, Z) :- parent(X, Y), parent(Y, Z).