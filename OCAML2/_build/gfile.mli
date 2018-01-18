(* Read a graph from a file,
 * Write a graph to a file. *)

open Graph

type path = string

(* Values are read as strings. *)
val from_file: path -> (string, string) graph

(* Similarly, we write only a (string,string) graph.
 * Use Graph.map if necessary to prepare the input graph. *)
val write_file: path -> (string, string) graph -> unit

(*this function takes a graph as argument and creates a file that can ben converted in .dot file so we can see the graph as a png*)
val export : path -> (string, string) graph -> unit
