open FordFulkerson
open Graph
open Gfile
open Printer

let main outfile source sink graph = 


let original_graph = graph in
	let flow =0 in 
		let path = find_augmented_path graph source sink in
	
			print_path source path;

			let rec update_graph graph result = function
				|[] -> graph
				|path -> let flow = (flow_from_path_int path) in
				Printf.printf "Actual total flow : %i\n\n%!" (result+flow);

				let graph2 = (augment_flow_along_path path original_graph graph flow source) in 
				update_backward_direction source graph2 flow path;
				let new_path = (find_augmented_path graph2 source sink) in
				print_path source new_path;
				update_graph graph2 (result+flow) new_path	
				in
			let result = 0 in
				let updated_graph = (update_graph graph result path) in
					let final_graph = map updated_graph (fun x->x) (string_of_int) in
						export outfile final_graph					


