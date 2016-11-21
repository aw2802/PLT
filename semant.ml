open Ast
open Sast

let classIndices: (string, int) Hashtbl.t =  Hashtbl.create 100

let createClassIndices cdecls=  
	let classHandler index cdecl=
	Hashtbl.add classIndices cdecl.cname index in (*scope handling is missing*)
	List.iteri classHandler cdecls

 (* Translates Ast to Sast *)
let check program = match program with
	 Program (classes) -> ignore (createClassIndices classes)
	| _ -> raise (Failure ("Invalid classs syntax"));

	let convertToSast classes =
		let convertClassToSast class_decl = 
			{scname = cdecl.cbody.cname;
			 svariables = class_decl.cbody.cvariables
			 smethods = class_decl.cbody.cmethods
			}
		in
		function -> List.map convertClassToSast classes
	in
	let sast = convertToSast classes in
	sast
