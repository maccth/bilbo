import "examples/graphs.bb"

type Vertex = colour

def init(g) = match g | [a] where not (#a is Vertex) => become [a::Vertex(0)]

def changeCol(g) =
    match g
    | [a,>,b] where (a..colour == b..colour) =>
        b..colour = b..colour + 1
        become [a,>,b]

vcolour = $init! |> $changeCol!

cols = G2 >> vcolour
print "Vertex colouring graph"
print cols