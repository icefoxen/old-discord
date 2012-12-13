open Discord

exception DiscordException of string

let labels2str pkg =
  List.fold_left (fun x y -> x ^ "|" ^ y) "labels " pkg.pkglabels
;;

let segment2str name vl =
  let countLines str =
    let i = ref 0 in
      for x = 0 to (String.length str) - 1 do
	if str.[x] = '\n' then
	  incr i
      done
  in
    
    if String.mem name '\n' then
      raise (DiscordException( "Segment names cannot contain newlines!" ))
    else
      let numLines = string_of_int (countLines str) in
	"segment " name ^ " " ^ numLines ^ "\n" ^
	  vl ^ "\n" ^ "endsegment\n"
;;
  

let segments2str pkg =
  ""
;;

let package2str pkg =
  "package " ^ pkg.pkgname ^ "\n" ^
    (labels2str pkg) ^ "\n" ^
    (segments2str pkg)
;;



let str2name str =
  ""
;;

let str2labels str =
  []
;;

let str2segment str =
  ("", "")
;;

let str2segments str =
  ()
;;



(* Now, for the finale... *)
let dumpToFile db filename =
  let c = open_out_bin filename in
    Marshal.to_channel c db [];
    close_out c;
;;

let readFromFile filename =
  print_endline filename;
  let c = open_in_bin filename in
  let db = (Marshal.from_channel c : package list ref) in
    close_in c;
    db
;;
