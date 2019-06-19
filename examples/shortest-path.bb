import "examples/graphs.bb"

type Place = set, dist

def init(g,startId) =
    match g
    | [n] where (&n == startId) and not (#n is Place)  => become [n::Place(True, 0)]
    | [n] where not (#n is Place) => become [n::Place(False, 0)]

def add(g) =
    match g
    | [x,w>,y] where x..set and not y..set =>
        y..set = True
        y..dist = x..dist+w
        become [x,w>,y]

def reduce(g) =
    match g
    | [x,w>,y] where y..dist > x..dist+w =>
        y..dist = x..dist+w
        become [x,w>,y]

def removeEdge(g) =
    match g
    | [x,<w>,y] => become [x,y]
    | [x,w>,y] => become [x,y]

shortest = $add! |> $reduce!

G' = (G, "a") >> $init!
dists =
    G'
    >> shortest
    |> $removeEdge!
    
print "Shortest path graph"
print dists