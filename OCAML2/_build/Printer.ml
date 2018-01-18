
let rec print_list = function 
|[] -> Printf.printf "\n\n%!";
| (e,id)::tail -> Printf.printf "%s %!" id ; print_list tail;; 

let rec print_vertex_list = function 
|[] -> Printf.printf "\n\n%!";
| x::tail -> Printf.printf "%s %!" x ; print_vertex_list tail;;

(*this function allows us to print a path*)
let rec print_path source = function 
	|[] -> Printf.printf "End of the program\n%!"
	|((e,id)::tail) -> Printf.printf "A path has been founded : \n%s %s %!" source id ; print_list tail;;
