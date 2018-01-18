open Graph
open FordFulkerson
open Main

let () =

  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  let infile = Sys.argv.(1)
  and _source = Sys.argv.(2)
  and _sink = Sys.argv.(3)
  and outfile = Sys.argv.(4) in

  let graph = Gfile.from_file infile in
	let graph2 = map graph (fun x -> x) (int_of_string) in 

(*use .write to write ina file .export to have a file that can be converted in .dot, graph 5 to have the result of Ford Fulkerson with bacwards directions and graph4 without*)
		(*Gfile.export outfile graph*)
		main outfile _source _sink graph2
		
  


