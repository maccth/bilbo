import "examples/graphs.bb"
import "examples/transitive-closure.bb"

def reachable(t,x,y) =
      match t 
      | [a,>,b] where (&a==x) and (&b==y) => return True 
      | _ => return False
      
tran = G2 >> closure

canReach = tran >> reachable

print "f is reachable from a"
print ("a", "f") >> canReach