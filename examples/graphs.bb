a = "a"::1
b = "b"::2
c = "c"::3
d = "d"::4
e = "e"::5
f = "f"::6
h = "h"::7

G = [a, 10>, b, 12>, c]
    + [d, 4>, b, 6>, e]
    + [a, 12>, d, 7>, c, 1>, e, 5>, a]
    + [e, 5>, h, 8>, f]

def removeWeight(g) = match g | [x,a>,y] => become [x,>,y]
// unweighted version
G2 = G >> $removeWeight!