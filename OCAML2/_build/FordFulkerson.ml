open Graph
open List

(*-------------------------------------------------------------------------*)

(* FOUR FUCTIONS THAT MAKES CODE EASIER *)

(*-------------------------------------------------------------------------*)

(*we use this function to choose between two path which one to return when we search for the augmented_path*)

let min_path x y =  if length x = 0 then y else 
	if length y = 0 then x else 
	if length x < length y then x else y;;

(*-------------------------------------------------------------------------*)

(*little function to know if one element is part of a list*)
let rec appartient x my_list = match my_list with
	|[] -> false 
	|e::tail -> if e = x then true else appartient x tail;;

(*-------------------------------------------------------------------------*)

(*little function to get the list of vertex in the path*)
let rec get_vertex_list path source = match path with
	|[] -> [source]
	|(e,id)::tail -> id::(get_vertex_list tail source);;

(*-------------------------------------------------------------------------*)

(*little function to get the list of vertex in outedges*)
let rec get_outedges_vertexes outedges = match outedges with
	|[] -> []
	|(e,id)::tail -> id::(get_outedges_vertexes tail);;

(*-------------------------------------------------------------------------*)


(* REAL START OF THE FUNCTIONS FOR FORD FULKERSON *)

(*-------------------------------------------------------------------------*)

let find_augmented_path graph source sink =

(*this function reach the sink node if possible return the path to it and takes care of not going through the same node twice so NO infinite loop is possible*)
  let rec reach_sink path visited = function
    |[] -> []
    |(e,id)::tail -> if appartient id visited then reach_sink path visited tail else 
		if id = sink then ((append path [(e,id)]) ) else(min_path (reach_sink (append path [(e,id)]) (id::visited) (find_vertex graph id).outedges) (reach_sink path (visited) tail) )
  in
	reach_sink [] [] ((find_vertex graph source).outedges);;

(*-------------------------------------------------------------------------*)

let flow_from_path_int path = 

(*function with accumulator min that allows us to returns the min cost of all edges of the path *)
	let rec get_min_flow min = function
	|[] -> min
	|(e,id)::tail -> if e<min then get_min_flow e tail else get_min_flow min tail in
	get_min_flow max_int path;;

(*-------------------------------------------------------------------------*)

let augment_flow_along_path path original_graph graph flow source =
	
(* these function are quite like the map function of graph.ml *)

(*we use the first to take care of the edges if they are part of the path (we substract the min flow to their label) if not we don't change them*)
 
	let rec update_edges current_id = function
			|[] -> []
			|(e,id)::tail -> let liste_init_outedges = get_outedges_vertexes ( (find_vertex original_graph current_id).outedges ) in

		if ( ( appartient (e,id) path ) && ( appartient id liste_init_outedges ) ) 			then 
			( if e-flow = 0 then update_edges current_id tail 
			else (e - flow,id)::(update_edges current_id tail) ) 
		else (e,id)::(update_edges current_id tail)

	in

(*we use the second function to apply the first one on outedges and inedges of a node*)

	  let update_vertex_info source = (fun x -> if (appartient x.id (get_vertex_list path source) ) then  ({ label= x.label ;
		                                id= x.id ;
		                                outedges= update_edges x.id x.outedges ;
		                                inedges= x.inedges })
								else ({ label= x.label ;
		                                id= x.id ;
		                                outedges= x.outedges ;
		                                inedges= x.inedges }) )
	  in

(* now we map the second function to all the nodes of the graph*)
	
		{vertices = map_hashtbl graph.vertices (update_vertex_info source)}

(*-------------------------------------------------------------------------*)

(*in this function we take all the edges of the path and we add the backward direction to the graph. If this edge already exist we add the flow to its initial label. The function add_edge_with_addition is a special version of add_edges and takes care of this all by itself for more precision go see it in graph.ml*)
 
let rec update_backward_direction source graph flow = function
	|[]->()
	|(e,id)::tail -> add_edge_with_addition graph id source flow;
	update_backward_direction id graph flow tail;
 























