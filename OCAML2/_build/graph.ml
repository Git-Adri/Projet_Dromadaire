type id = string

type ('v, 'e) vertex_info =
  { label: 'v ;
    id: id ;
    outedges: ('e * id) list ;
    inedges: ('e * id) list }

type ('v, 'e) graph =
  { (* Hashtable mapping identifiers to vertex_info. *)
    vertices: (id, ('v, 'e) vertex_info) Hashtbl.t ;
  }    

let find_vertex graph id =
  try Hashtbl.find graph.vertices id
  with Not_found -> failwith ("Vertex " ^ id ^ " cannot be found in this graph.")

let find_edge graph id1 id2 =
  try
    let info = find_vertex graph id1 in
    let (e, _) = List.find (fun (_, id) -> id = id2) info.outedges in
    Some e
  with Not_found -> None

(**************  CONSTRUCTORS  **************)

let new_graph () = { vertices = Hashtbl.create 60 }

let add_vertex graph label id =
  if Hashtbl.mem graph.vertices id then
    let info = Hashtbl.find graph.vertices id in
    Hashtbl.replace graph.vertices id { info with label }
  else
    Hashtbl.add graph.vertices id { label ; id ; outedges = [] ; inedges = [] }

(* Insert or replace a pair (label * id) in a list. *)
let rec insert acu id label = function
  | [] -> (label, id) :: acu
  | (_, id2) as pair :: rest ->
    if id = id2 then List.rev_append rest ((label, id) :: acu)
    else insert (pair :: acu) id label rest


let add_edge graph id1 id2 elabel =

  (* Get vertex_info of origin and destination. *)
  let origin_info = find_vertex graph id1
  and dest_info   = find_vertex graph id2 in


  (* Add (or replace) the given edge. *)
  let new_outedges = insert [] id2 elabel origin_info.outedges
  and new_inedges  = insert [] id1 elabel dest_info.inedges in


  (* Put it back in the hashtable. *)
  Hashtbl.replace graph.vertices id1 { origin_info with outedges = new_outedges } ;
  Hashtbl.replace graph.vertices id2 { dest_info with inedges = new_inedges } ; 

  (* Done. *)
  ()

(* Insert or replace a pair (label * id) in a list with (ancient label + new label * id). *)
let rec insert_and_add acu id label = function
  | [] -> (label, id) :: acu
  | (x, id2) as pair :: rest -> (*this is where we add the last label + the new one*)
    if id = id2 then ( (List.rev_append rest ((label+x, id) :: acu) ) )
    else (insert_and_add (pair :: acu) id label rest ) 

let add_edge_with_addition graph id1 id2 elabel =

  (* Get vertex_info of origin and destination. *)
  let origin_info = find_vertex graph id1
  and dest_info   = find_vertex graph id2 in


  (* Add (or replace) the given edge. *)
  let new_outedges = insert_and_add [] id2 elabel origin_info.outedges in
	let new_inedges  = insert_and_add [] id1 elabel dest_info.inedges in

  (* Put it back in the hashtable. *)
  Hashtbl.replace graph.vertices id1 { origin_info with outedges = new_outedges } ;
  Hashtbl.replace graph.vertices id2 { dest_info with inedges = new_inedges } ; 

  (* Done. *)
  ()


(**************  COMBINATORS, ITERATORS  **************)

let v_iter graph f = Hashtbl.iter (fun _ info -> f info) graph.vertices

(* Creates a new hashtable by mapping f to every value. *)
let map_hashtbl h f =
  let result = Hashtbl.create (Hashtbl.length h) in
  Hashtbl.iter (fun key v -> Hashtbl.add result key (f v)) h ;
  result

let map graph vmap emap =
  
  let rec map_edges = function
    |[] -> []
    |(e,id)::tail -> (emap e,id)::(map_edges tail)
  in
  
  let map_vertex_info = (fun x -> { label= (vmap x.label) ;
                                    id= x.id ;
                                    outedges= map_edges x.outedges  ;
                                    inedges= map_edges x.inedges })
  in

  {vertices = map_hashtbl graph.vertices map_vertex_info}
  
