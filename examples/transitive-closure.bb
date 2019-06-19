import "examples/graphs.bb"

def link(g) =
    match g
    | [a,>,b,>,c] and not [a,>,c] => become [a,>,b,>,c] + [a,>,c]

closure = $link!

tran = G2 >> closure
print "Transitive closure graph"
print tran