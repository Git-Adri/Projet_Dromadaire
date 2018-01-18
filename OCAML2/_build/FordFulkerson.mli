open Graph

val get_vertex_list :(int * id) list -> id -> id list 

(*this function find an augmented path in a graph if there is one and return it, if there is no path it returns empty list []*)
val find_augmented_path :('v, 'e) graph -> id -> id -> ('e * id) list 

(*this function returns the min flow of the path given in argument*)
val flow_from_path_int : (int * id) list -> int 

(*this function update the graph knowing the path used and the min flow of this path*)
val augment_flow_along_path: (int * id) list -> ('v, int) graph -> ('v, int) graph -> int -> id -> ('v, int) graph

(*this function creates the backwards directions knowing the path used and the min flow of this path (use this after augment_flow_along_path) *)
val update_backward_direction: Graph.id -> ('v, int) graph -> int -> (int * id) list -> unit
