(* discordprog.ml
   Because having a separate library is nice.
*)

let usagemessage =
"Discord source-database thingy.
Usage: discord <dbfile> <option>
  -h          This message
  -i pkgfile  Import package into the database
  -o pkgname  Output package into file
  -r pkgname  Remove package from database

  -sl label      Lists all packages with the given label
  -al label pkg  Adds a label to a package
  -rl label pkg  Removes a label from a package

  -d             Dumps the entire database to text files
  
  -os pkg seg    Outputs a given segment of a package
  -is pkg seg    Imports a given segment
  -rs pkg seg    Removes a given segment

  -l          Lists all the packages in the database
  -lv <pkg>   Lists packages verbosely (argument is optional)
  -ll pkg     Lists all labels a package has
  -ls pkg     Lists the names of all segments in a package
"


let printUsage () =
  print_endline usagemessage
;;



(* Utility functions *)

let areYouSure () =
  print_string "Are you sure? y/N ";
  let response = read_line () in
    if response.[0] = 'y' or response.[0] = 'Y' then
      true
    else
      false
;;

let callWithTwoArgs func str =
  ()
;;

let slurpFile filename =
  Printf.printf "Slurping file %s\n" filename;
  let f = open_in_bin filename
  and s = ref "" in
    try 
      while true do
	s := !s ^ (input_line f) ^ "\n"
      done;
      !s
    with
	End_of_file -> close_in f;       print_endline "done"; !s
;;


let getArg n =
  try
    Sys.argv.(n+1)
  with
      _ -> Printf.eprintf "Not enough args!"; exit 1;
;;


(* Real functions *)


let newDB () = 
  let fn = getArg 1 in
    Discord.dumpToFile [] fn
;;


let newPackage () =
  let dbname = getArg 1 
  and pkgname = getArg 2 in
  let db = Discord.readFromFile dbname in

  let newdb = Discord.addPackage db (Discord.makePackage pkgname ) in
    Discord.dumpToFile newdb dbname
;;
  

(* For this to work, we need a function to read in a package from a file *)
let importPackage () =
(*  Discord.addPackage  *)
  ()
;;

(* And, same here really. *)
let exportPackage () =
  ()
;;


(* XXX *)
let removePackage () =
  let dbname = getArg 1
  and pkgname = getArg 2 in
  let db = Discord.readFromFile dbname in
  if areYouSure () then (
    print_endline "Find a sane way to implement this, please.";
      let newdb = List.filter (fun x -> (Discord.getName x) <> pkgname) !db in
	Discord.dumpToFile (ref newdb) dbname
    )
;;


let listLabel () =
  let dbname = getArg 1 
  and label = getArg 2 in
  let db = Discord.readFromFile dbname in
  let packages = Discord.getPackagesWithLabel !db label in
    List.iter (fun x -> print_endline (Discord.getName x)) packages
;;

let addLabel () =
  let dbname = getArg 1 
  and pkgname = getArg 2 
  and label = getArg 3 in
  let db = Discord.readFromFile dbname in

  let pkg = Discord.getPackage db pkgname in
    Discord.addLabel pkg label;
    Discord.dumpToFile db dbname
;;

let removeLabel () =
  let dbname = getArg 1 
  and pkgname = getArg 2 
  and label = getArg 3 in
  let db = Discord.readFromFile dbname in

  let pkg = Discord.getPackage db pkgname in
    Discord.delLabel pkg label;
    Discord.dumpToFile db dbname;
;;


let dump () =
  ()
;;


let exportSeg () =
  let dbname = getArg 1 
  and pkgname = getArg 2 
  and seg = getArg 3 in
  let db = Discord.readFromFile dbname in

  let pkg = Discord.getPackage db pkgname in
    print_endline (Discord.getSegment pkg seg)
;;

let importSeg () =
  let dbname = getArg 1 
  and pkgname = getArg 2 
  and seg = getArg 3 in
  let db = Discord.readFromFile dbname in

    print_endline "Done done";
  let segtext = slurpFile seg in
  let pkg = Discord.getPackage db pkgname in
    print_endline "Done done done";

    Discord.addSegment pkg seg segtext;
    Discord.dumpToFile db dbname
;;

let removeSeg () =
  let dbname = getArg 1 
  and pkgname = getArg 2 
  and seg = getArg 3 in
  let db = Discord.readFromFile dbname in

  if areYouSure () then (
    let pkg = Discord.getPackage db pkgname in
      Discord.delSegment pkg seg;
      Discord.dumpToFile db dbname;
    )
;;

(* These should be able to take an argument for a package, or a list,
   or nothing and just list everything...
*)
let listAll () =
  let dbname = getArg 1 in
  let db = Discord.readFromFile dbname in
  List.iter (fun x -> print_endline (Discord.getName x)) !db
;;

let listVerbosely () =
  listAll ()
;;

let listPkgLabels () =
  let dbname = getArg 1 
  and pkgname = getArg 2 in
  let db = Discord.readFromFile dbname in

  let pkg = Discord.getPackage db pkgname in
    List.iter (fun x -> print_endline x) (Discord.getLabels pkg)
;;

let listPkgSegments () =
  let dbname = getArg 1 
  and pkgname = getArg 2 in
  let db = Discord.readFromFile dbname in

  let pkg = Discord.getPackage db pkgname in
    List.iter (fun x -> print_endline x) (Discord.getAllSegmentNames pkg)
;;






let _ = 
  if Array.length Sys.argv < 2 then
    printUsage ()
  else
    let cmd = Sys.argv.(1) in
      match cmd with
	  "n" | "new" -> newPackage ()
	| "i" | "import" -> importPackage ()
	| "x" | "export" -> exportPackage ()
	| "r" | "rem" | "remove" -> removePackage ()

	| "ll" | "listlabel" -> listLabel ()
	| "al" | "addlabel" -> addLabel ()
	| "rl" | "removelabel" -> removeLabel ()

	| "nd" | "newdb" -> newDB ()
	| "d" | "dump" -> dump ()

	| "is" | "importseg" | "importsegment" -> importSeg ()
	| "xs" | "exportseg" | "exportsegment" -> exportSeg ()
	| "rs" | "remseg" | "removeseg" | "removesegment" -> removeSeg ()

	| "l" | "list" -> listAll ()
	| "lv" | "listverbose" -> listVerbosely ()
	| "lp" | "listlabels" -> listPkgLabels ()
	| "ls" | "listseg" | "listsegments" -> listPkgSegments ()

	| _ -> printUsage ()

	    



(*
  let db = Discord.readFromFile Sys.argv.(1) in
  let speclist = [
      (* Huzzah for partial function application! *)

      ("-h", Arg.Unit( printUsage ), "Show help");
      ("-i", Arg.String( importPackage db ), "Import package");
      ("-o", Arg.String( outputPackage db ), "Output package");
      ("-r", Arg.String( removePackage db ), "Remove package");

      ("-sl", Arg.String( listLabel db ), "List packages with label");
      ("-al", Arg.Rest( callWithTwoArgs (addLabel db) ), "Add label");
      ("-rl", Arg.Rest( callWithTwoArgs (delLabel db) ), "Remove label");

      ("-d", Arg.Unit( dump db ), "Dump database");

      ("-os", Arg.Rest( callWithTwoArgs (outputSegment db) ), 
        "Output package segment");
      ("-is", Arg.Rest( callWithTwoArgs (inputSegment db) ), 
        "Import package segment");
      ("-rs", Arg.Rest( callWithTwoArgs (deleteSegment db) ), 
        "Remove package segment");

      ("-l", Arg.Unit( listAll db ), "List packages");
      ("-lv", Arg.Rest( listVerbosely db ), "List package verbosely");
      ("-ll", Arg.String( listLabels db ), "List package labels");
      ("-ls", Arg.String( listSegments db ), "List package segments");
    ] in

    Arg.parse speclist printUsage ""
*)
;;
