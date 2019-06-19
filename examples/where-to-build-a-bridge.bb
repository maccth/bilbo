import "examples/shortest-path.bb"
import "examples/graphs.bb"

def addBridge(g) =
    match g
    | [a,x>,b,y>,c] =>
        w = 0.75*(x+y)
        bridge = [a,w>,c]
        become [a,x>,b,y>,c] + bridge

def findDistToLocation(loc, g) =
    match g
    | [x] where &x == loc => return x..dist

distToC = "c" >> findDistToLocation

G' = (G, "a") >> $init!

bestG =
    G'
    >> addBridge
    |> shortest
    |> min by distToC

print "Where to build a bridge?"
print bestG
