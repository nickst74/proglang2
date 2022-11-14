datatype ''a Tree = Node of ''a * ''a Tree list;
datatype ''a option = None | Some of ''a * ''a Tree list;


fun leftish 0 = Node (0, nil)
|   leftish n = 
    let
        val rc = Node (n, nil)
        val lc = leftish (n-1)
    in
      Node (n, [lc, rc])
    end

fun same_fringe t1 t2 =
    let
      fun aux l1 l2 =
        let
            fun nextLeaves nil = None
            |   nextLeaves (x::xs) = case x of
                  Node(w, nil)  => Some (w, xs)
                | Node(_, kids)  => nextLeaves (kids @ xs)

            val ls1 = nextLeaves l1
            val ls2 = nextLeaves l2
        in
            case (ls1, ls2) of
              (None, None) => true
            | (None, _) => false
            | (_,  None) => false
            | (Some(x1, k1), Some(x2, k2)) => (x1=x2) andalso aux k1 k2
        end
    in
        aux [t1] [t2]
    end